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
          | ThreeOfAKind [Card]
          | Straight [Card]
          | Flush [Card]
          | FullHouse [Card]
          | FourOfAKind [Card]
          | StraightFlush [Card]
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


rSortBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
rSortBy f = sortBy (flip f)

(>>.) :: (a -> b) -> (b -> c) -> (a -> c)
(>>.) = flip (.)

hand :: String -> Hand
hand =   cards
       >>. rSortBy (comparing value)
       >>. groupBy (same value)
       >>. rSortBy (comparing length)
       >>. ranking 
       >>. promoteStraight
       >>. promoteFlush    

ranking :: [[Card]] -> Hand
ranking [[a,b,c,d],[e]]       = FourOfAKind [a,b,c,d,e]
ranking [[a,b,c],[d,e]]       = FullHouse [a,b,c,d,e]
ranking [[a,b,c],[d],[e]]     = ThreeOfAKind [a,b,c,d,e]
ranking [[a,b],[c],[d],[e]]   = Pair     [a,b,c,d,e]
ranking [[a],[b],[c],[d],[e]] = HighCard [a,b,c,d,e] 

cards :: String -> [Card]
cards = map card . words 

promoteStraight :: Hand -> Hand
promoteStraight (HighCard [a,b,c,d,e]) 
    | value a - value e == 4 = Straight [a,b,c,d,e]
promoteStraight (HighCard [a,b,c,d,e]) 
    | value a == 14 &&  value b == 5 = Straight [b,c,d,e,a]
promoteStraight h = h


promoteFlush :: Hand -> Hand
promoteFlush (HighCard cs) | flush cs = Flush cs
promoteFlush (Straight cs) | flush cs = StraightFlush cs
promoteFlush h = h
