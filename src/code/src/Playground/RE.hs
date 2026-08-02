{-# LANGUAGE OverloadedLists #-}
module Playground.RE where

import Control.Monad.State.Lazy (State, execState, modify, runState, evalState, state)
import Data.Bifunctor (bimap, first, second)
import Data.Functor.Identity
import Data.List (union, nub, sort, singleton, intercalate)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe, isJust, fromJust)

import Playground.Search (bfs, bfsM, never, neverM, reachable, statefulBfs)

-- Regular expression
data RE = Empty
        | Epsilon
        | Let Char
        | RE :|: RE
        | RE :*: RE
        | Kleene RE

infixl 6 :|: 
infixl 7 :*: 

-- Pretty printing for regular expressions
instance Show RE where
  showsPrec _ Empty       = showString "\x2205"
  showsPrec _ Epsilon     = showString "\x03b5"
  showsPrec _ (Let c)     = showChar c
  showsPrec p (r1 :*: r2) = showParen (p > 7) (showsPrec 7 r1 . showsPrec 7 r2)
  showsPrec p (r1 :|: r2) = showParen (p > 6) (showsPrec 6 r1 . showChar '|' . showsPrec 7 r2)
  showsPrec p (Kleene r)  = showsPrec 9 r . showChar '*'

-- 'ab|c*'
re1 :: RE
re1 = Let 'a' :*: Let 'b' :|: Kleene (Let 'c')

-- (a*(bc)*)|d
re2 :: RE
re2 = Kleene (Kleene (Let 'a') :*: Kleene (Let 'b' :*: Let 'c')) :|: Let 'd'


type TransitionMap s c = Map s (Map c [s])


-- Finite automaton
data FA s c = FA { states :: [s]
                 , transitions :: TransitionMap s c
                 , start :: s
                 , finals :: [s]
                 }
  deriving Show

mapState :: Ord s2 => (s1 -> s2) -> FA s1 c -> FA s2 c
mapState f (FA qs delta s fs) = 
  FA (map f qs)
     (Map.mapKeys f (fmap (fmap (fmap f)) delta))
     (f s)
     (map f fs)


-- Epsilon NFA
type EpsNFA = FA Int (Maybe Char)

-- NFA
type NFA = FA Int Char

-- Intermediate powerset DFA that has lists as states
type PDFA = FA [Int] Char

-- DFA
type DFA = FA Int Char


-- Checks if a state is a final state
isFinal :: Eq s => FA s c -> s -> Bool
isFinal fa q = q `elem` finals fa

-- Lookups if any targets can be reached from a state via a letter
lookupT :: (Ord c, Ord s) => s -> c -> TransitionMap s c -> [s]
lookupT s c tm = Map.findWithDefault [] c (Map.findWithDefault Map.empty s tm)

unionT :: (Ord c, Ord s) => TransitionMap s c -> TransitionMap s c -> TransitionMap s c
unionT = Map.unionWith (Map.unionWith union)

unionsT :: (Ord c, Ord s) => [TransitionMap s c] -> TransitionMap s c
unionsT = foldr unionT []

trans :: (Ord c, Ord s) => s -> c -> [s] -> TransitionMap s c
trans s c ts = [(s, [(c, ts)])]

eps :: (Ord c, Ord s) => s -> [s] -> TransitionMap s (Maybe c)
eps s ts = trans s Nothing ts

-- Returns a fresh state
newState :: State [Int] Int
newState = state (\(q : qs) -> (q, qs)) 

-- Construct an epsilon NFA from a regular expression
thompson :: RE -> EpsNFA
thompson r = evalState (go r) [0..]
  where
    go Empty       = do 
      q <- newState
      return (FA [q] [] q [])
    go Epsilon     = do 
      q <- newState
      return (FA [q] [] q [q])
    go (Let c)     = do 
      q1 <- newState 
      q2 <- newState 
      return (FA [q1, q2] (trans q1 (Just c) [q2]) q1 [q2])
    go (r1 :|: r2) = do
      enfa1 <- go r1
      enfa2 <- go r2
      q1 <- newState
      return (FA (q1 : (states enfa1 ++ states enfa2))
                 (eps q1 [start enfa1, start enfa2] 
                   `unionT` transitions enfa1 
                   `unionT` transitions enfa2)
                 q1
                 (finals enfa1 ++ finals enfa2))
    go (r1 :*: r2) = do
      enfa1 <- go r1
      enfa2 <- go r2
      return (FA (states enfa1 ++ states enfa2)
                 (unionsT [eps f [start enfa2] | f <- finals enfa1] 
                   `unionT` transitions enfa1 
                   `unionT` transitions enfa2)
                 (start enfa1)
                 (finals enfa2))
    go (Kleene r) = do
      enfa <- go r
      q1 <- newState
      q2 <- newState
      return (FA (q1 : q2 : states enfa)
                 (eps q1 [start enfa] 
                   `unionT` eps q2 [q1] 
                   `unionT` unionsT [eps f [q2] | f <- finals enfa] 
                   `unionT` transitions enfa)
                 q1
                 [q1, q2])



-- Compute epsilon closure from a set of states in an epsilon NFA
--
-- A BFS that traverses the epsilon NFA while keeping track of all visited
-- states that were reachable via epsilon transitions.
epsilonClosure :: EpsNFA -> Int -> [Int]
epsilonClosure enfa q = snd (statefulBfs next neverM q [])
  where
    next q = do 
      modify (q:)
      return (lookupT q Nothing (transitions enfa))


-- Removes all epsilon transitions from an epsilon NFA and converts
-- the automaton into an NFA
epsilonElim :: EpsNFA -> NFA
epsilonElim enfa = FA newStates newTransitions (start enfa) newFinals
  where 
    -- Memoize epsilon closures for each state
    epsilonClosures = Map.fromList (map (\q -> (q, epsilonClosure enfa q)) (states enfa))

    -- newTransitions has transitions for each state has non-epsilon transitions 
    -- transition. During the construction of newTransitions, we only loose
    -- states that have only epsilon transition. Thus, we do not miss any states
    -- here.
    newStates = Map.keys newTransitions 

    -- Collect all transitions go from a state in the epsilon closure to some
    -- target via a letter and merge all sets of transitions from a given state
    -- with the same letter.
    newTransitions = 
      unionsT [[(q, m')] | q <- states enfa
                         , let ec = epsilonClosures Map.! q
                         , (v, m) <- Map.toList (transitions enfa)
                         , let m' = Map.mapKeysMonotonic fromJust (Map.filterKeys isJust m)
                         , v `elem` ec
                         ]

    -- Final states are all original states that have a final state in their
    -- epsilon closure
    newFinals = [q | q <- states enfa 
                   , let ec = epsilonClosures Map.! q
                   , any (isFinal enfa) ec
                   ]


-- Constructs an intermediate DFA via the powerset construction from an NFA.
--
-- We use a BFS to find all states that can be reached in the powerset.
-- [-1] is a sink.
powerset :: NFA -> PDFA
powerset nfa = FA newStates'
                  newTransitions
                  [start nfa]
                  (filter (any (`elem` finals nfa)) newStates)
  where
    ((_, newStates), newTransitions) = statefulBfs next neverM [start nfa] []

    -- add sink if it was not introduced during the search
    newStates' = [[-1] | any (== [-1]) newStates] ++ newStates

    -- We assume that all letters of the alphabet occured as a label of
    -- transition.
    alph =  nub [c | m <- Map.elems (transitions nfa), c <- Map.keys m]

    succs qs c = 
      case nub . sort . concatMap (\q -> lookupT q c (transitions nfa)) $ qs of
        [] -> [-1]  -- sink
        ts -> ts

    next qs = do 
      let tm = unionsT [trans (sort qs) c [succs qs c] | c <- alph]
      modify (tm `unionT`)
      return [t | m <- Map.elems tm, ts <- Map.elems m, t <- ts]


-- Converts an intermediate DFA into a DFA
relabel :: PDFA -> DFA 
relabel pdfa = mapState label pdfa
  where label = (Map.fromList (zip (states pdfa) [0..]) Map.!)

-- Constructs a DFA from a regular expression
compile :: RE -> DFA
compile = relabel . powerset . epsilonElim . thompson


-- Checks is a word is a member of the language of an regular expression
member :: String -> RE -> Bool
member w r = reachable next stop (w, start a)
  where 
    a = compile r
    
    stop ("", q) = isFinal a q
    stop _       = False

    next ([], q)   = []
    next (c:cs, q) = map (cs,) (lookupT q c (transitions a))


graphviz :: (Eq s, Show c, Show s) => FA s c -> String
graphviz (FA qs delta s fs) = 
  unlines [ "digraph {"
          , "  node [shape=circle];"
          , "  -1 [style=invis];\n  -1 -> " ++ show s ++ ";"
          , intercalate "\n" nodes
          , intercalate "\n" edges
          , "}"
          ]
  where
    nodes = map (\q -> "  " ++ node q ++ ";") qs

    node q | q `elem` fs = show q ++ " [shape=doublecircle]"
           | otherwise   = show q
          
    edges = ["  " ++ show s ++ " -> " ++ show t ++ " [label=\"" ++ show c ++ "\"];" | (s, m) <- Map.assocs delta, (c, ts) <- Map.assocs m, t <- ts]


-- | Checks if the language of a given regular expression is 'nullable'. A
-- language is called nullable, if it contains the empty word.
nullable :: RE -> Bool
nullable Empty         = False
nullable Epsilon       = True
nullable (Kleene _)    = True
nullable (re1 :|: re2) = nullable re1 || nullable re2
nullable (re1 :*: re2) = nullable re1 && nullable re2
nullable _             = False

-- | Computes the Brzozowski derivative w.r.t. a letter.
brzozowski :: Char -> RE -> RE
brzozowski a Empty = Empty
brzozowski a Epsilon = Empty
brzozowski a (Let b)
  | a == b    = Epsilon
  | otherwise = Empty
brzozowski a (re1 :|: re2) = brzozowski a re1 :|: brzozowski a re2
brzozowski a (re1 :*: re2)
  | nullable re1 = re1' :*: re2 :|: re2'
  | otherwise    = re1' :*: re2
  where 
    re1' = brzozowski a re1
    re2' = brzozowski a re2
brzozowski a (Kleene re) = brzozowski a re :*: Kleene re

-- | Computes the Brzozowski derivative w.r.t. a word.
derivative :: String -> RE -> RE
derivative w re = foldl (flip brzozowski) re w

