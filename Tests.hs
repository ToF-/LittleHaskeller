module Tests
where 
import Test.HUnit
import PokerHand

main = runTestTT $ TestList 
       [compare (card "6h") (card "6s") ~?= EQ]