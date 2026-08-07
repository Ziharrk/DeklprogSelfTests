{-# LANGUAGE OverloadedLists #-}
module Playground.Language.FA where

import Control.Monad.State.Lazy (State, execState, modify, evalState, state)
import Data.Bifunctor (bimap, first, second)
import Data.List (union, nub, sort, singleton, intercalate)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, fromJust, listToMaybe)

import Playground.Language.RegExp (RegExp(..))
#ifdef TEMPLATE
-- import Playground.Search (bfs, bfsM, never, neverM, reachable, statefulBfs)
#else
import Playground.Search (bfs, bfsM, never, neverM, reachable, statefulBfs)
#endif


-- | Transition map.
type TransitionMap s c = Map s (Map c (Set s))

-- | Finite automaton.
data FA s c = FA { states :: Set s
                 , transitions :: TransitionMap s c
                 , start :: s
                 , finals :: Set s
                 }
  deriving Show


-- | Get the dot code of a finite automaton.
--
-- You can paste the code to https://dreampuf.github.io/GraphvizOnline/ to
-- inspect your automaton for debugging or entertainment.
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
    nodes = map (\q -> "  " ++ node q ++ ";") (Set.toList qs)

    node q | q `elem` fs = show q ++ " [shape=doublecircle]"
           | otherwise   = show q
          
    edges = 
      ["  " ++ show s ++ " -> " ++ show t ++ " [label=\"" ++ show c ++ "\"];" 
         | (s, m) <- Map.assocs delta
         , (c, ts) <- Map.assocs m
         , t <- Set.toList ts
         ]

-- | Map each state to a different one.
mapState :: Ord s2 => (s1 -> s2) -> FA s1 c -> FA s2 c
mapState f (FA qs delta s fs) = 
  FA (Set.map f qs)
     (Map.mapKeys f (fmap (fmap (Set.map f)) delta))
     (f s)
     (Set.map f fs)

-- | Map each edge label to a different one.
mapTrans :: Ord c2 => (c1 -> c2) -> FA s c1 -> FA s c2
mapTrans f fa = fa { transitions = Map.map (\m -> Map.mapKeys f m) (transitions fa) }

-- | Epsilon NFA
type EpsNFA = FA Int (Maybe Char)

-- | NFA
type NFA = FA Int Char

-- | Intermediate powerset DFA that has lists as states
type PDFA = FA [Int] Char

-- | DFA
type DFA = FA Int Char


-- | Checks if a state is a final state.
isFinal :: Ord s => FA s c -> s -> Bool
isFinal fa q = q `Set.member` finals fa

-- | Lookups if any targets can be reached from a state via a letter.
lookupT :: (Ord c, Ord s) => s -> c -> TransitionMap s c -> Set s
lookupT s c tm = Map.findWithDefault [] c (Map.findWithDefault Map.empty s tm)

-- | Computes the union of two transitions maps. If both maps include a mapping
-- of @(q, c)@ to some states @ts1@ and @ts2@, then the resulting map will 
-- include a mapping of @(q, c)@ to @union ts1 ts2@.
unionT :: (Ord c, Ord s) => TransitionMap s c -> TransitionMap s c -> TransitionMap s c
unionT = Map.unionWith (Map.unionWith Set.union)

-- | Same as 'unionT' but for multiple maps.
unionsT :: (Ord c, Ord s) => [TransitionMap s c] -> TransitionMap s c
unionsT = foldr unionT []

-- | Constructs a map including one transition.
trans :: (Ord c, Ord s) => s -> c -> Set s -> TransitionMap s c
trans s c ts = [(s, [(c, ts)])]

-- | Constructs a map including one epsilon transition.
eps :: (Ord c, Ord s) => s -> Set s -> TransitionMap s (Maybe c)
eps s ts = trans s Nothing ts


-- | Returns a fresh state
newState :: State [Int] Int
newState = state (\(q : qs) -> (q, qs)) 

-- | Construct an epsilon NFA from a regular expression
thompson :: RegExp -> EpsNFA
thompson r = evalState (go r) [0..]
#if TEMPLATE
  where
    go = do undefined
#else
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
      return (FA ([q1] `Set.union` states enfa1 `Set.union` states enfa2)
                 (eps q1 [start enfa1, start enfa2] 
                   `unionT` transitions enfa1 
                   `unionT` transitions enfa2)
                 q1
                 (finals enfa1 `Set.union` finals enfa2))
    go (r1 :*: r2) = do
      enfa1 <- go r1
      enfa2 <- go r2
      return (FA (states enfa1 `Set.union` states enfa2)
                 (unionsT [eps f [start enfa2] | f <- Set.toList (finals enfa1)] 
                   `unionT` transitions enfa1 
                   `unionT` transitions enfa2)
                 (start enfa1)
                 (finals enfa2))
    go (Kleene r) = do
      enfa <- go r
      q1 <- newState
      q2 <- newState
      return (FA ([q1, q2] `Set.union` states enfa)
                 (eps q1 [start enfa] 
                   `unionT` eps q2 [q1] 
                   `unionT` unionsT [eps f [q2] | f <- Set.toList (finals enfa)] 
                   `unionT` transitions enfa)
                 q1
                 [q1, q2])
#endif


-- | Compute epsilon closure from a set of states in an epsilon NFA.
epsilonClosure :: EpsNFA -> Int -> Set Int
#ifdef TEMPLATE
epsilonClosure = undefined
#else
epsilonClosure enfa q = snd (statefulBfs next neverM q [])
  where
    next q = do 
      modify (Set.insert q)
      return (Set.toList (lookupT q Nothing (transitions enfa)))
#endif


-- | Removes all epsilon transitions from an epsilon NFA and converts
-- the automaton into an NFA
epsilonElim :: EpsNFA -> NFA
#ifdef TEMPLATE
epsilonElim = undefined
#else
epsilonElim enfa = FA newStates newTransitions (start enfa) newFinals
  where 
    -- Memoize epsilon closures for each state
    epsilonClosures = Map.fromList (map (\q -> (q, epsilonClosure enfa q)) (Set.toList (states enfa)))

    -- newTransitions has transitions for each state has non-epsilon transitions 
    -- transition. During the construction of newTransitions, we only loose
    -- states that have only epsilon transition. Thus, we do not miss any states
    -- here.
    newStates = Set.fromList (Map.keys newTransitions)

    nonEpsTransitions = Map.mapKeysMonotonic fromJust . Map.filterKeys isJust

    -- Collect all transitions go from a state in the epsilon closure to some
    -- target via a letter and merge all sets of transitions from a given state
    -- with the same letter.
    newTransitions = 
      unionsT [[(q, m')] | q <- Set.toList (states enfa)
                         , let ec = epsilonClosures Map.! q
                         , (v, m) <- Map.toList (transitions enfa)
                         , let m' = nonEpsTransitions m
                         , v `Set.member` ec
                         ]

    -- Final states are all original states that have a final state in their
    -- epsilon closure
    newFinals = Set.filter (any (isFinal enfa) . (epsilonClosures Map.!)) (states enfa)
#endif


-- | Constructs an intermediate DFA via the powerset construction from an NFA.
powerset :: NFA -> PDFA
#if TEMPLATE
powerset = undefined
#else
powerset nfa = FA newStates'
                  newTransitions
                  [start nfa]
                  (Set.filter (any (`elem` finals nfa)) newStates')
  where
    ((_, newStates), newTransitions) = statefulBfs next neverM [start nfa] []

    -- Add sink if it was not introduced during the search
    newStates' = Set.fromList [[-1] | any (== [-1]) newStates] `Set.union` Set.fromList newStates

    -- We assume that all letters of the alphabet occured as a label of
    -- transition.
    alph =  nub [c | m <- Map.elems (transitions nfa), c <- Map.keys m]

    succs qs c = 
      let ts = Set.unions [lookupT q c (transitions nfa) | q <- qs]
       in if Set.null ts then [[-1]] else [Set.toList ts]

    next qs = do 
      -- qs is canonicalized here since it doubles as this state's label
      -- in the powerset DFA; order must not matter
      let tm = unionsT [trans (sort qs) c (succs qs c) | c <- alph]
      modify (tm `unionT`)
      return [t | m <- Map.elems tm, ts <- Map.elems m, t <- Set.toList ts]
#endif


-- | Converts an intermediate DFA into a DFA.
relabel :: PDFA -> DFA 
#ifdef TEMPLATE
relabel = undefined
#else
relabel pdfa = mapState label pdfa
  where label = (Map.fromList (zip (Set.toList (states pdfa)) [0..]) Map.!)
#endif


-- | Constructs a DFA from a regular expression.
compile :: RegExp -> DFA
#ifdef TEMPLATE
compile = undefined
#else
compile = relabel . powerset . epsilonElim . thompson
#endif


-- | Checks is a word is a member of the language of an regular expression.
member :: String -> DFA -> Bool
#ifdef TEMPLATE
member = undefined
#else
member w a = reachable next stop (w, start a)
  where 
    stop ("", q) = isFinal a q
    stop _       = False

    next ([], q)   = []
    next (c:cs, q) = map (cs,) (Set.toList (lookupT q c (transitions a)))
#endif


stateElim :: DFA -> RegExp
#ifdef TEMPLATE
stateElim = error "not implemented"
#else
stateElim dfa@(FA qs delta s fs) = Map.keys (Map.elems (transitions (foldr elim gnfa qs)) !! 0) !! 0
  where
    -- Two states that are not used in the original DFA
    s' = maximum qs + 1
    f' = maximum qs + 2

    -- Sipser's GNFA state-elimination procedure
    elim q gnfa = gnfa { states = remaining
                       , transitions = mergeParallel (others `unionT` bridgeOverQ) }
      where
        -- Every state except q
        remaining = Set.filter (/= q) (states gnfa)

        -- q's own self-loop (if it exists) becomes a Kleene star that gets
        -- spliced into every path that used to pass through q
        selfLoop = 
          listToMaybe [ c | (c, ts') <- Map.assocs (Map.findWithDefault Map.empty q (transitions gnfa))
                          , t <- Set.toList ts'
                          , t == q
                          ]

        splice re1 Nothing        re3 = re1 :*: re3
        splice re1 (Just re2)     re3 = re1 :*: Kleene re2 :*: re3

        -- For every predecessor s of q and successor t of q, add a direct
        -- direct transition s -> t labeled prefix selfLoop* suffix
        bridgeOverQ =
          unionsT [ trans s (splice prefix selfLoop suffix)  [t] 
                    | (s, m) <- Map.assocs (transitions gnfa)
                    , s /= q
                    , (prefix, ts) <- Map.assocs m
                    , q `elem` ts
                    , (suffix, ts') <- Map.assocs (Map.findWithDefault Map.empty q (transitions gnfa))
                    , t <- Set.toList ts'
                    , t /= q
                    ]

        -- Drop every edge incident to q
        others =
          Map.map (Map.map (Set.filter (/= q))) (Map.delete q (transitions gnfa))

        -- If bridging created two edges s -> t, merge them into one by 
        -- alternation
        mergeParallel tm =
          unionsT [ trans s (foldr1 (:|:) res) [t]
                  | ((s, t), res) <- Map.assocs grouped
                  ]
          where
            grouped = Map.fromListWith (++)
                        [ ((s, t), [re])
                        | (s, m)  <- Map.assocs tm
                        , (re, ts) <- Map.assocs m
                        , t <- Set.toList ts
                        ]

    -- Convert DFA into a generalized NFA by converting transition labels
    -- into regular expressions and introducing two new states acting as start
    -- and final state.
    gnfa = FA { states = Set.insert s' (Set.insert f' qs)
              , transitions = trans s' Epsilon [s]
                  `unionT` unionsT [trans f Epsilon [f'] | f <- Set.toList fs]
                  `unionT` transitions (mapTrans Let dfa)
              , start = s'
              , finals = [f']
              }
#endif

