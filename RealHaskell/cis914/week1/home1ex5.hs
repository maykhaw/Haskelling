import Test.QuickCheck 
type Peg = String 
type Move = (Peg, Peg) 
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move] 
hanoi 0 pegA pegB pegC = []
hanoi n pegA pegB pegC = hanoi (n-1) pegA pegC pegB ++ [(pegA,pegC)] ++ hanoi (n-1) pegB pegA pegC

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move] 
hanoi4 0 pegA pegB pegC pegD = [] 
hanoi4 n pegA pegB pegC pegD = hanoi (n-1) 

testhanoi :: Integer -> Bool 
testhanoi x = length (hanoi x pegA pegB pegC) >= length (hanoi4 x pegA pegB pegC pegD)

main = do
    quickCheck testhanoi
