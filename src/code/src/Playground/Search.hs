module Playground.Search (pruningM, searchM, dfsM, bfsM) where

import Control.Monad (filterM)
import Data.Functor.Identity
import Data.List (insert, singleton, uncons)
import Data.Maybe (fromJust)


-- |Reconstructs a path from a leaf node and parent map.
-- The process terminates if a node v can be reached such that parent v = v
trace :: Eq a => a -> a -> (a -> a) -> [a]
#ifdef TEMPLATE
trace = error "not implemented"
#else
trace start end parent = start : reverse (takeWhile (/= start) (iterate parent end))
#endif

-- |Takes a 'next' function and removes all next states that satisfy a predicate
pruningM :: Monad m => (a -> m [a]) -> (a -> m Bool) -> a -> m [a]
pruningM next pred curr = filterM (fmap not . pred) =<< next curr

-- |An abstract search algorithm.
searchM :: (Eq a, Monad m)
  => (a -> [a] -> [a])
  -- ^ Pushes a new state into the set of states to visit next
  -> ([a] -> Maybe (a, [a]))
  -- ^ Pops the next state to visit
  -> (a -> m [a])
  -- ^ Determines which states to consider next (may include states that were
  -- already visited)
  -> (a -> m Bool)
  -- ^ Stops search if a criterion is met
  -> a
  -- ^ Starting state
  -> m (Maybe [a], [a])
  -- ^ Path from the starting state to the last state, if the stopping criterion
  -- was satisfied, and all visited states
#ifdef TEMPLATE
searchM = error "not implemented"
#else
searchM push pop next found start = go [start] start [] [start]
  where
    go states prev parent visited =
      case pop states of
        Nothing               -> return (Nothing, visited)
        Just (curr, states') -> do
          let parent' = (curr, prev) : parent
          end <- found curr
          if end
            then return (Just (trace start curr (fromJust . flip lookup parent')), visited)
            else do
              unvisited <- next `pruningM` (return . (`Prelude.elem` visited)) $ curr
              let states'' = foldr push states' unvisited
              go states'' curr parent' (unvisited ++ visited)
#endif

-- |Non-monadic variant of 'searchM'.
search :: Eq a
  => (a -> [a] -> [a])
  -> ([a] -> Maybe (a, [a]))
  -> (a -> [a])
  -> (a -> Bool)
  -> a
  -> (Maybe [a], [a])
#ifdef TEMPLATE
search = error "not implemented"
#else
search push pop next found = runIdentity . searchM push pop (Identity . next) (Identity . found)
#endif

-- |Checks if an element is in a list.
elem :: Eq a => a -> [a] -> Bool 
#ifdef TEMPLATE
elem = error "not implemented"
#else
elem x xs = 
  case search (:) uncons (\i -> if i < length xs then [i + 1] else []) (\i -> xs !! i == x) 0 of
    (Just _, _) -> True
    _           -> False
#endif

-- |DFS
dfsM :: (Eq a, Monad m) => (a -> m [a]) -> (a -> m Bool) -> a -> m (Maybe [a], [a])
#ifdef TEMPLATE
dfsM = error "not implemented"
#else
dfsM = searchM (:) uncons
#endif

-- |BFS
bfsM :: (Eq a, Monad m) => (a -> m [a]) -> (a -> m Bool) -> a -> m (Maybe [a], [a])
#ifdef TEMPLATE
bfsM = error "not implemented"
#else
bfsM = searchM (\x xs -> xs ++ [x]) uncons
#endif

-- Example from challenge
g :: [[(Int, Int)]]
g = [ [(23, 1), (8, 2)]
    , [(1, 2), (5, 3)]
    , [(30, 3)]
    , []
    ]

#ifdef TEMPLATE
-- |Shortest path algorithm which visited nodes during traversal
shortestPath :: [[(Int, Int)]] -> Int -> Int -> Maybe Int
shortestPath graph from to =
  let result = search push pop next stop (0, from)
   in case result of
        (Nothing,   _) -> Nothing
        (Just path, _) -> Just (fst (last path))
  where
    push (d, v) pq
      | v `notElem` map snd pq = insert (d, v) pq
      | otherwise = map (\(d', w) -> if v == w then (min d d', v) else (d', w)) pq
    pop = uncons
    next (d, v) = map (\(w, u) -> (d + w, u)) (graph !! v)
    stop (_, v) = v == to
#else
-- |Shortest path algorithm which visited nodes during traversal
shortestPath :: [[(Int, Int)]] -> Int -> Int -> IO (Maybe Int)
shortestPath graph from to = do
  result <- searchM push pop next stop (0, from)
  case result of
    (Nothing,   _) -> return Nothing 
    (Just path, _) -> return (Just (fst (last path)))
  where
    push (d, v) pq
      | v `notElem` map snd pq = insert (d, v) pq
      | otherwise = map (\(d', w) -> if v == w then (min d d', v) else (d', w)) pq
    pop = uncons
    next (d, v) = do print v
                     return (map (\(w, u) -> (d + w, u)) (graph !! v))
    stop (_, v) = return (v == to)
#endif

