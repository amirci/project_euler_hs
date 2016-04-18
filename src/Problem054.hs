module Problem054 where

import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace

data Player = Player1 | Player2 deriving (Eq, Show)

type Hand = [String]
type HandValue = String

cards = ['0'..'9'] ++ ['T', 'J', 'Q', 'K', 'A']

cardEq a b = fromJust $ pure compare <*> indexOf a <*> indexOf b
  where indexOf c = findIndex (== c) cards

highToLow = sortBy (flip cardEq)

highFreq (a, f1) (b, f2) = foq $ compare f1 f2
  where
    foq EQ = cardEq a b
    foq x  = x

handValue :: Hand -> HandValue
handValue hand
  | royal    && flush = "900000"       -- Royal flush: Ten, Jack, Queen, King, Ace, in same suit
  | straight && flush = "8" ++ values' -- Straight Flush: All cards are consecutive values of same suit
  | flush             = "5" ++ values' -- Same suit
  | straight          = "4" ++ values' -- straight : consecutive values
  | otherwise         = freqCheck $ sortBy (flip highFreq) $ map toPair $ group values
  where
    values = sortBy cardEq $ map head hand
    values' = reverse values
    toPair l = (head l, length l)

    royal = isSuffixOf values cards 
    flush = (== 1) $ length $ nub $ map last hand
    straight = isInfixOf values cards

    -- one pair
    freqCheck [(p1, 2), (c1, 1), (c2, 1), (c3, 1)] = "10" ++ [p1] ++ (highToLow [c1, c2, c3])
    -- two pairs
    freqCheck [(p1, 2), (p2, 2), (c1, 1)] = "200" ++ (highToLow [p1, p2]) ++ [c1]
    -- three of a kind
    freqCheck [(t1, 3), (c1, 1), (c2, 1)] = "300" ++ [t1] ++ (highToLow [c1, c2])
    -- full house: 3 same kind and a pair
    freqCheck [(t1, 3), (p1, 2)] = "6000" ++ [t1, p1]
    -- four of a kind
    freqCheck [(t1, 4), (c1, 1)] = "7000" ++ [t1, c1]
    -- highest card
    freqCheck _ = "0" ++ values'

playHand :: String -> Player
playHand hand = winner $ zip h1 h2
  where
    pairs = splitOn " " hand
    h1 = handValue $ take 5 pairs
    h2 = handValue $ drop 5 pairs
    winner [] = Player2
    winner ((a, b):xs)
      | eq == GT  = Player1
      | eq == EQ  = winner xs
      | otherwise = Player2
      where 
        eq = cardEq a b
