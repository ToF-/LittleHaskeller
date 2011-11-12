module Main
where
import PokerHand

main = getContents 
       >>= lines
       >>. scores
       >>. unlines
       >>. putStrLn

