module PokerHand
where
import Char

data Card = C Value deriving (Ord,Eq)
type Value = Int

card :: String -> Card
card [c,_] = C $ (ord c) - (ord '0')
