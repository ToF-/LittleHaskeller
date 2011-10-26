module PokerHand
where
import Char

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



