module Tests
where 
import Test.HUnit
import PokerHand
import Data.Ord (comparing)
import Data.List (sort,sortBy)

ud = words "A♥ 2♥ T♥ K♥ 9♥ Q♥ J♥"
sd = words "2♥ 9♥ T♥ J♥ Q♥ K♥ A♥"

main = runTestTT $ TestList 
       [sortBy (comparing card) ud  ~?= sd
       ,map suit (cards "A♣ A♦ A♥ A♠") ~?= ['♣','♦','♥','♠']
       ,flush (cards "A♥ T♥ 3♥ 4♥ 2♥") ~?= True
       ,flush (cards "A♦ T♥ 3♥ 4♥ 2s") ~?= False
       ,flush (cards "A♦ T♦ 3♦ 4♦ 2♦") ~?= True]
    where cards = map card . words

