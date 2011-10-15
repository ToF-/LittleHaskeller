module Tests
where 
import Test.HUnit
import PokerHand
import Data.Ord (comparing)

main = runTestTT $ TestList 
       [comparing card "6h" "6s" ~?= EQ
       ,comparing card "6h" "5s" ~?= GT]
