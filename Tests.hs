module Tests
where 
import Test.HUnit
import PokerHand
import Data.Ord (comparing)
import Data.List (sort,sortBy)

ud = words "A♣ 2♣ T♣ K♣ 9♣ Q♣ J♣"
sd = words "2♣ 9♣ T♣ J♣ Q♣ K♣ A♣"

main = runTestTT $ TestList 
       [sortBy (comparing card) ud  ~?= sd
       ,map suit (cards "A♣ A♦ A♥ A♠") ~?= "♣♦♥♠"
       ,flush (cards "A♣ T♣ 3♣ 4♣ 2♣") ~?= True
       ,flush (cards "A♠ T♣ 3♣ 4♣ 2♣") ~?= False
       ,flush (cards "A♠ T♠ 3♠ 4♠ 2♠") ~?= True
       ,"6♣ 4♦ A♣ 3♠ K♠" `beat` "8♥ J♥ 7♦ 5♥ 6♣"
       ,"5♥ 2♦ 3♥ 4♦ 2♥" `beat` "A♥ K♥ Q♦ J♦ 9♥"
       ,"5♥ 4♦ 3♥ 2♦ 3♣" `beat` "A♥ K♥ Q♦ J♦ 9♥"
       ,"5♥ 4♦ 3♥ 3♣ 2♥" `beat` "7♦ 5♥ 3♦ 2♠ 2♦"
       ,"2♦ 2♣ 3♣ 3♠ 4♥" `beat` "A♥ A♠ K♣ Q♦ J♠"  
       ,"2♦ 2♣ 2♠ 3♥ 4♦" `beat` "A♥ A♠ K♣ K♦ J♠"
       ,"A♦ Q♦ 6♦ J♦ 2♦" `beat` "K♠ Q♥ J♠ T♥ 9♠"
       ,"2♦ 2♣ 3♦ 3♠ 3♥" `beat` "A♦ Q♦ 6♦ J♦ 2♦"
       ,"2♦ 2♠ 2♥ 2♣ 3♦" `beat` "A♥ A♦ A♠ K♥ K♠"
       ,"6♠ 5♦ 4♣ 3♦ 2♥" `beat` "A♣ A♥ A♦ K♣ Q♠"
       ,"5♠ 4♦ 3♣ 2♦ A♥" `beat` "A♣ A♥ A♦ K♣ Q♠"
       ,"6♥ 4♥ 3♥ 2♥ A♥" `beat` "A♠ K♣ Q♥ J♠ T♦"
       ,"5♥ 4♥ 3♥ 2♥ A♥" `beat` "A♦ A♠ A♥ A♠ K♥"
       ,"6♥ 5♥ 4♥ 3♥ 2♥" `beat` "A♦ A♠ A♥ A♠ K♥"
       ,show HighCard      ~?= "High Card"
       ,show Pair          ~?= "Pair"
       ,show TwoPairs      ~?= "Two Pairs"
       ,show ThreeOfAKind  ~?= "Three of a Kind"
       ,show Straight      ~?= "Straight"
       ,show Flush         ~?= "Flush"
       ,show FullHouse     ~?= "Full House"
       ,show FourOfAKind   ~?= "Four of a Kind"
       ,show StraightFlush ~?= "Straight Flush"
       ,maxRanking "6♥ 6♦ 6♠ 6♣ K♠ K♦" ~?= Nothing
       ,maxRanking "T♦ 6♣ 4♦ A♣ 3♠ K♠ 2♦" ~?= Just HighCard
       ,maxRanking "6♣ 6♦ A♣ 3♠ K♠ 5♦ 7♥" ~?= Just Pair
       ,maxRanking "9♣ A♥ K♠ K♣ K♦ 9♦ 6♦" ~?= 
                   Just FullHouse
       ,markResults [Nothing] ~?= [""]
       ,markResults [Nothing, Just Pair] ~?= ["","Pair (winner)"]
       ,markResults [Nothing, Just Pair, Just HighCard] ~?= 
                        ["","Pair (winner)","High Card"]
       ,scores ["6♥ 6♦ 6♠ 6♣",
                "6♣ 4♦ A♣ 3♠ K♠ 5♦ T♠",
                "6♣ 6♦ A♣ 3♠ K♠",
                "9♣ A♥ K♠ 3♣ K♦ 9♦ 6♦"] ~?= 
                   ["6♥ 6♦ 6♠ 6♣",
                    "6♣ 4♦ A♣ 3♠ K♠ 5♦ T♠ High Card",
                    "6♣ 6♦ A♣ 3♠ K♠" ,
                    "9♣ A♥ K♠ 3♣ K♦ 9♦ 6♦ Two Pairs (winner)"]
       ]
           where
      beat h g = comparing (hand . cards) h g ~?= GT
