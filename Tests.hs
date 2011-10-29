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
       ,map suit (cards "A♣ A♦ A♥ A♠") ~?= ['♣','♦','♥','♠']
       ,flush (cards "A♣ T♣ 3♣ 4♣ 2♣") ~?= True
       ,flush (cards "A♠ T♣ 3♣ 4♣ 2♣") ~?= False
       ,flush (cards "A♠ T♠ 3♠ 4♠ 2♠") ~?= True
       ,"6♣ 4♦ A♣ 3♠ K♠" `beat` "8♥ J♥ 7♦ 5♥ 6♣"
       ,"5♥ 2♦ 3♥ 4♦ 2♥" `beat` "A♥ K♥ Q♦ J♦ T♥"
       ,"5♥ 4♦ 3♥ 2♦ 3♣" `beat` "A♥ K♥ Q♦ J♦ T♥"]
    where beat h g = comparing hand h g ~?= GT



