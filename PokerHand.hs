module PokerHand
where
import Char
import Data.Ord
import Data.List

data Card = C { value :: Value, suit :: Suit } 
            deriving (Ord,Eq)
type Value = Int
type Suit = Char

data Hand = H Ranking [Card]
            deriving (Ord,Eq)

data Ranking = HighCard
             | Pair
             | TwoPairs
             | ThreeOfAKind
             | Straight
             | Flush
             | FullHouse
             | FourOfAKind
             | StraightFlush
            deriving (Ord,Eq)

instance (Show) Ranking
    where
      show HighCard      = "High Card"
      show Pair          = "Pair" 
      show TwoPairs      = "Two Pairs" 
      show ThreeOfAKind  = "Three of a Kind" 
      show Straight      = "Straight" 
      show Flush         = "Flush" 
      show FullHouse     = "Full House" 
      show FourOfAKind   = "Four of a Kind"
      show StraightFlush = "Straight Flush"


ranking :: Hand -> Ranking
ranking (H r _) = r

maxRanking :: String -> Maybe Ranking
maxRanking s = case (subs (cards s)) of
                  [] -> Nothing
                  hs -> Just (max hs)
    where 
      max = maximum . map ranking . map hand
      subs = filter ((5 ==) . length) . subsequences 


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

hand :: [Card] -> Hand
hand = rSortBy (comparing value)
       >>. groupBy (same value)
       >>. rSortBy (comparing length)
       >>. rank
       >>. promoteStraight
       >>. promoteFlush    

rank :: [[Card]] -> Hand
rank gs = H (calcRank gs) (concat gs)  
    where calcRank [[_,_,_,_],_]   = FourOfAKind 
          calcRank [[_,_,_],_]     = FullHouse
          calcRank [[_,_,_],_,_]   = ThreeOfAKind
          calcRank [[_,_],[_,_],_] = TwoPairs
          calcRank [[_,_],_,_,_]   = Pair    
          calcRank [_,_,_,_,_]     = HighCard 

cards :: String -> [Card]
cards = map card . words 

promoteStraight :: Hand -> Hand
promoteStraight (H r [a,b,c,d,e]) 
    | value a - value e == 4 = 
        H Straight [a,b,c,d,e]
promoteStraight (H HighCard [a,b,c,d,e]) 
    | value a == 14 && value b == 5 = 
        H Straight [b,c,d,e,a]
promoteStraight h = h


promoteFlush :: Hand -> Hand
promoteFlush (H HighCard cs) 
    | flush cs = H Flush cs
promoteFlush (H Straight cs) 
    | flush cs = H StraightFlush cs
promoteFlush h = h

