module Problem054 where

import Data.List.Split
import Data.List
import Data.Maybe

data Player = Player1 | Player2 deriving (Eq, Show)

type Hand = [String]

cardEq a b = fromJust $ pure compare <*> indexOf a <*> indexOf b
  where
    cards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']
    indexOf c = findIndex (== c) cards

highToLow = reverse . (sortBy cardEq)

handValue :: Hand -> String
handValue hand = value freq
  where
    toPair l = (head l, length l)
    freq = map toPair $ group $ sortBy cardEq $ map head hand
    -- one pair
    value [(p1, 2), (c1, 1), (c2, 1), (c3, 1)] = "10" ++ [p1] ++ (highToLow [c1, c2, c3])
    -- two pairs
    value [(p1, 2), (p2, 2), (c1, 1)] = "200" ++ (highToLow [p1, p2]) ++ [c1]
    -- three of a kind
    value [(t1, 3), (c1, 1), (c2, 1)] = "300" ++ [t1] ++ (highToLow [c1, c2])
    -- straight : consecutive values
    -- flush: same suit
    -- full house: 3 same kind and a pair
    value [(t1, 3), (p1, 2)] = "6000" ++ [t1, p1]
    -- four of a kind
    value [(t1, 4), (c1, 1)] = "7000" ++ [t1, c1]
    -- highest card
    value cards = highToLow $ map fst cards

playHand :: Hand -> Hand -> Player
playHand h1 h2 = Player2
  where
    v1 = handValue h1
    v2 = handValue h2
    result = compareBy highToLow v1 v2

winner :: String -> Player
winner cards = playHand h1 h2
  where 
    pairs = splitOn " " cards
    h1 = take 5 pairs
    h2 = drop 5 pairs
