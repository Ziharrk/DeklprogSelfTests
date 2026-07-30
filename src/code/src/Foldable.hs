module Foldable where


import Prelude hiding (all, any)

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving Show

foldTree :: r -> (r -> a -> r -> r) -> Tree a -> r
foldTree fempty _     Empty        = fempty
foldTree fempty fnode (Node l x r) = fnode (foldTree' l) x (foldTree' r)
  where foldTree' = foldTree fempty fnode


any ::  (a -> Bool) -> Tree a -> Bool
#ifdef TEMPLATE
any = error "not implemented"
#else
any p = foldTree False (\l x r -> l || p x || r)
#endif

all ::  (a -> Bool) -> Tree a -> Bool
#ifdef TEMPLATE
all = error "not implemented"
#else
all p = foldTree False (\l x r -> l && p x && r)
#endif

elem :: Int -> Tree Int -> Bool
#ifdef TEMPLATE
elem = error "not implemented"
#else
elem x = any (== x)
#endif

notElem :: Int -> Tree Int -> Bool
#ifdef TEMPLATE
notElem = error "not implemented"
#else
notElem x = all (/= x)
#endif

null :: Tree a -> Bool
#ifdef TEMPLATE
null = error "not implemented"
#else
null = any (const True)
#endif

length :: Tree a -> Int
#ifdef TEMPLATE
length = error "not implemented"
#else
length = foldTree 0 (\l x r -> l + r + 1)
#endif

maximum :: Tree Int -> Int
#ifdef TEMPLATE
maximum = error "not implemented"
#else
maximum = foldTree minBound (\l x r -> max l (max x r))
#endif

minimum :: Tree Int -> Int
#ifdef TEMPLATE
minimum = error "not implemented"
#else
minimum = foldTree maxBound (\l x r -> min l (min x r))
#endif

sum :: Tree Int -> Int
#ifdef TEMPLATE
sum = error "not implemented"
#else
sum = foldTree 0 (\l x r -> l + x + r)
#endif

product :: Tree Int -> Int
#ifdef TEMPLATE
product = error "not implemented"
#else
product = foldTree 1 (\l x r -> l * x * r)
#endif

