module PokerHand
where

data Card = C deriving (Ord,Eq)

card :: String -> Card
card _ = C
