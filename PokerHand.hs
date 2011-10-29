module PokerHand
where
import Char
import Data.List

data Card = C { value :: Value, suit :: Suit } 
            deriving (Ord,Eq)
type Value = Int
type Suit = Char

data Hand = HighCard [Card]
          | Pair  deriving (Ord,Eq)

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
flush (c:cs) = all (same suit c) cs

same :: (Eq a) => (t -> a) -> t -> t -> Bool
same f a b = f a == f b

hand :: String -> Hand
hand s = case length gs  of
           4 -> Pair
           5 -> HighCard cs 
       where cs = sortBy (flip compare) $ cards s
             gs = groupBy (same value) cs

cards :: String -> [Card]
cards = map card . words 
