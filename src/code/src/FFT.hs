module FFT
  ( fft
  , ifft
  , polymul
  ) where

import Data.Complex


#ifndef TEMPLATE
fft :: [Complex Double] -> [Complex Double]
fft [x] = [x]
fft xs = zipWith (+) evenPart twiddles ++ zipWith (-) evenPart twiddles
  where
    n = length xs
    (evens, odds) = split xs
    evenPart = fft evens
    oddPart = fft odds
    twiddles = zipWith (*) oddPart [cis (-2 * pi * fromIntegral k / fromIntegral n) | k <- [0..n `div` 2 - 1]]

split :: [a] -> ([a], [a])
split []       = ([], [])
split [x]      = ([x], [])
split (x:y:xs) = let (xs1, xs2) = split xs
                  in (x : xs1, y : xs2)

ifft :: [Complex Double] -> [Complex Double]
ifft xs = (map (/ fromIntegral n) . map conjugate . fft . map conjugate) xs
  where n = length xs

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n = case dropWhile (< n) (iterate (* 2) 1) of
                      (p : _) -> p
                      _       -> error "should not happen"

pad :: Integral a => Int -> [a] -> [Complex Double]
pad n xs = map ((:+ 0) . fromIntegral) xs ++ replicate (n - length xs) 0
#endif
polymul :: Integral a => [a] -> [a] -> [a]
#ifdef TEMPLATE
polymul = error "not implemented"
#else
polymul p q = take (fromIntegral m) (map (round . realPart) (ifft (zipWith (*) fp fq)))
  where
    m = length p + length q - 1
    n = nextPowerOfTwo m
    fp = fft (pad n p)
    fq = fft (pad n q)
#endif

