module PokerHand
where
import Char

data Card = C Value deriving (Ord,Eq)
type Value = Int

card :: String -> Card
card ['A',_] = C 14
card ['K',_] = C 13
card ['Q',_] = C 12
card ['J',_] = C 11
card ['T',_] = C 10
card [c,_] = C $ (ord c) - (ord '0')
