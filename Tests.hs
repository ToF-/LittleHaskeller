module Tests
where 
import Test.HUnit
import PokerHand
import Data.Ord (comparing)
import Data.List (sortBy)

deck = words "2h 9h Th Jh Qh Kh Ah"
main = runTestTT $ TestList 
       [sortBy (comparing card) deck  ~?= deck]


