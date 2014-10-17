import Data.Char hiding (isLower, isUpper)
import Test.QuickCheck 

let2int :: Char -> Int 
let2int c = ord c - ord 'a' 

let2intUpper :: Char -> Int 
let2intUpper c = ord c - ord 'A' 

int2let :: Int -> Char 
int2let n = chr (ord 'a' + n) 

int2letUpper :: Int -> Char 
int2letUpper n = chr (ord 'A' + n) 

isLower :: Char -> Bool 
isLower x = x >= 'a' && x <= 'z' 

isUpper :: Char -> Bool 
isUpper x = x >= 'A' && x <= 'Z' 

shift :: Int -> Char -> Char 
shift n c | isLower c = int2let ((let2int c+n) `mod` 26)
	  | isUpper c = int2letUpper ((let2intUpper c+n) `mod` 26)
	  | otherwise = c 

encode :: Int -> String -> String 
encode n xs = [shift n x | x <- xs] 

testcode :: Int -> String -> Bool 
testcode n s = s == encode (26-n) (encode n s)

testletter :: Char -> Int -> Property 
testletter c n = n `mod` 26 /= 0 && (isLower c || isUpper c) ==> encode n [c] /= [c] 

testupdown :: String -> Int -> Bool 
testupdown c n = encode n (map switchcase c) == map switchcase (encode n c) 
switchcase c | isLower c = toUpper c 
	     | isUpper c = toLower c 
	     | otherwise = c 

main = do
	quickCheck testcode
	quickCheck testletter  
	quickCheck testupdown 
