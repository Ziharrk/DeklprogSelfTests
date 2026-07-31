module Playground.Queue 
  ( Queue
  , empty
  , isEmpty
  , front
  , enqueue
  , dequeue
  ) where


-- |Queue.
data Queue a = Q [a] [a]

instance Show a => Show (Queue a) where
  show (Q as bs) = "Queue " ++ show (as ++ reverse bs)


-- |Empty queue.
empty :: Queue a
#ifdef TEMPLATE
empty = error "not implemented"
#else
empty = Q [] []
#endif

-- |Checks if a queue is empty.
isEmpty :: Queue a -> Bool
#ifdef TEMPLATE
isEmpty = error "not implemented"
#else
isEmpty (Q [] _) = True
isEmpty _        = False
#endif

-- |Head of a non-empty queue.
front :: Queue a -> a
#ifdef TEMPLATE
front = error "not implemented"
#else
front (Q (x : _) _) = x
front _             = error "empty queue"
#endif

-- |Helper function that enforces the invariant.
invariant :: Queue a -> Queue a
#ifdef TEMPLATE
invariant = error "not implemented"
#else
invariant (Q [] ys) = Q (reverse ys) []
invariant q         = q
#endif

-- |Adds an element to a queue.
enqueue :: a -> Queue a -> Queue a
#ifdef TEMPLATE
enqueue = undefined
#else
enqueue x (Q xs ys) = invariant (Q xs (x : ys))
#endif

-- |Removes an element from a non-empty queue.
dequeue :: Queue a -> Queue a
#ifdef TEMPLATE
dequeue = error "not implemented"
#else
dequeue (Q (_:xs) ys) = invariant (Q xs ys)
dequeue _             = error "empty queue"
#endif

