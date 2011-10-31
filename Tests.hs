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
       ,"5♥ 2♦ 3♥ 4♦ 2♥" `beat` "A♥ K♥ Q♦ J♦ 9♥"
       ,"5♥ 4♦ 3♥ 2♦ 3♣" `beat` "A♥ K♥ Q♦ J♦ 9♥"
       ,"5♥ 4♦ 3♥ 3♣ 2♥" `beat` "7♦ 5♥ 3♦ 2♠ 2♦"
       ,"2♦ 2♣ 2♠ 3♥ 4♦" `beat` "A♥ A♠ K♣ Q♦ J♠"
       ,"2♦ 2♠ 2♥ 2♣ 3♦" `beat` "A♥ A♦ A♠ K♥ K♠"
       ,"6♠ 5♦ 4♣ 3♦ 2♥" `beat` "A♣ A♥ A♦ K♣ Q♠"
       ,"5♠ 4♦ 3♣ 2♦ A♥" `beat` "A♣ A♥ A♦ K♣ Q♠"
       ,"6♥ 4♥ 3♥ 2♥ A♥" `beat` "A♠ K♣ Q♥ J♠ T♦"
       ,"5♥ 4♥ 3♥ 2♥ A♥" `beat` "A♦ A♠ A♥ A♠ K♥"
       ,"6♥ 5♥ 4♥ 3♥ 2♥" `beat` "A♦ A♠ A♥ A♠ K♥"]
    where beat h g = comparing hand h g ~?= GT



