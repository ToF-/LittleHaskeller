module Main
where
import PokerHand

main = putStrLn . unlines . scores . lines =<< getContents
