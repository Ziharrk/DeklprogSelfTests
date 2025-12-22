module BlackJack where

import Control.Monad
import Data.List (splitAt)
import Data.Maybe (fromJust)
import System.Random



data Player = Guest | Bank
  deriving Show


type Deck = [Card]


gameOver :: Hand -> Bool
gameOver h = getValue h > 21


winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest || getValue guest <= getValue bank && not (gameOver bank) = Bank
  | otherwise                                                                = Guest


shuffle :: Deck -> IO Deck
shuffle deck = newStdGen >>= return . shuffle' deck []
  where
    shuffle' [] h _ = h
    shuffle' cs h g = let (i  , g'    ) = randomR (0, length cs - 1) g
                          (cs', c:cs'') = splitAt i cs
                      in shuffle' (cs' ++ cs'') (c:h) g'


draw :: Deck -> Hand -> (Deck, Hand)
draw []       _ = error "empty deck"
draw (c : cs) h = (cs, c : h)


playBank :: Deck -> Hand -> Hand
playBank deck hand
  | getValue hand >= 17 = hand
  | otherwise           = uncurry playBank (draw deck hand)


data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving Show


data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Enum, Eq, Show)


data Card = Card Suit Rank
  deriving Show


getCardValue :: Card -> Int
getCardValue (Card _ r) = value r
  where
    value (Numeric v) = v
    value Jack        = 10
    value Queen       = 10
    value King        = 10
    value Ace         = 11


type Hand = [Card]


(<+>) :: Deck -> Deck -> Deck
(<+>) = (++)


fullDeck :: Deck
fullDeck = [ Card s r
           | s <- [Clubs ..]
           , r <- map Numeric [2 .. 10] ++ [Jack, Queen, King, Ace]
           ]


numOfAces :: Hand -> Int
numOfAces = length . filter isAce
  where
    isAce (Card _ Ace) = True
    isAce _            = False


getValue :: Hand -> Int
getValue h = adjust (sum (map getCardValue h)) (numOfAces h)
  where
    adjust v 0             = v
    adjust v a | v > 21    = adjust (v - 10) (a - 1)
               | otherwise = v


type Strategy = (Double -> Double, Hand -> Hand -> Bool, Double -> Bool)


-- play one round of blackjack
game :: Double -> Strategy -> IO Double
game x s@(bet, draw, end) = do
  c1 : c2 : c3 : c4 : deck <- shuffle fullDeck
  let b              = bet x
      (deck', guest) = drawing deck s [c1, c2] [c3]
  when (b < 1.0) (error "bet is too small")
  if getValue guest == 21
    then return (x + 1.5 * b)
    else do
      let bank = playBank deck' [c3, c4]
      case winner guest bank of
        Guest -> return (x + b)
        Bank  -> return (x - b)


-- draw as long as the strategy permits
drawing :: Deck -> Strategy -> Hand -> Hand -> (Deck, Hand)
drawing []       _              guest bank = ([], guest)
drawing (c : cs) s@(_, draw, _) guest bank
  | draw guest bank = drawing cs s (c : guest) bank
  | otherwise       = (cs, guest)


-- play at least 20 rounds of blackjack given a bankroll and a strategy
play :: Int -> Double -> Strategy -> IO Double
play n x s@(_, _, end)
  | n >= 20 && end x = return x
  | otherwise        = game x s >>= \y -> play (n + 1) y s


-- simulate 100 games of blackjack
benchmark :: Strategy -> IO Double
benchmark s = do
  let n = 100
  xs <- replicateM n (play 0 100.0 s)
  return (sum xs / fromIntegral n)


-- simple strategy (always bet 1, always draw a third,
-- stop if bankroll drops below 50)
simpleStrategy :: Strategy
simpleStrategy = (bet, draw, end)
  where
    bet x = 1.0

    draw guest bank
      | length guest <= 2    = True
      | otherwise            = False

    end x = x < 50.0

