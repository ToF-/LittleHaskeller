module PokerHand
where
import Char
import Data.List

data Card = C { value :: Value, suit :: Suit } 
            deriving (Ord,Eq)
type Value = Int
type Suit = Char

card :: String -> Card
card [v,s] = C (toValue v) s
    where 
      toValue 'A' = 14
      toValue 'K' = 13
      toValue 'Q' = 12
      toValue 'J' = 11
      toValue 'T' = 10
      toValue  c  = ((ord c) - (ord '0'))

flush :: [Card] -> Bool
flush (c:cs) = all (\x -> suit x == suit c) cs

data Hand = HighCard [Card]
          | Pair  deriving (Ord,Eq)

hand :: String -> Hand
hand "5♥ 4♦ 3♥ 2♦ 2♥" = Pair 
hand s = HighCard $ sortBy (flip compare) $ cards s

cards :: String -> [Card]
cards = map card . words 
