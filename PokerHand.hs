module PokerHand
where
import Char
import Data.Ord
import Data.List

data Card = C { value :: Value, suit :: Suit } 
            deriving (Ord,Eq)
type Value = Int
type Suit = Char

data Hand = HighCard [Card]
          | Pair     [Card]
            deriving (Ord,Eq)

card :: String -> Card
card [v,s] = C (toValue v) s
    where 
      toValue 'A' = 14
      toValue 'K' = 13
      toValue 'Q' = 12
      toValue 'J' = 11
      toValue 'T' = 10
      toValue  c  = ((ord c) - (ord '0'))

same :: (Eq a) => (t -> a) -> t -> t -> Bool
same f a b = f a == f b

flush :: [Card] -> Bool
flush (c:cs) = all (same suit c) cs

hand :: String -> Hand
hand s = case gs  of
           [[a,b],[c],[d],[e]] -> Pair [a,b,c,d,e]
           [_,_,_,_,_] -> HighCard cs 
       where cs = sortBy (flip compare) $ cards s
             gs = sortBy (flip groupSize) $ groupBy (same value) cs
             groupSize  = comparing length 

cards :: String -> [Card]
cards = map card . words 
