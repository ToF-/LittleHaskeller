module PokerHand
where
import Char

data Card = C Value Suit deriving (Ord,Eq)
type Value = Int


card :: String -> Card
card [v,s] = C (value v) s
    where 
      value 'A' = 14
      value 'K' = 13
      value 'Q' = 12
      value 'J' = 11
      value 'T' = 10
      value  c  = ((ord c) - (ord '0'))

flush :: [Card] -> Bool
flush (c:cs) = all (\x -> suit x == suit c) cs

type Suit = Char

suit :: Card -> Suit
suit (C _ s) = s


