import Test.QuickCheck 
import Data.Char
type Bit = Int 

bin2int :: [Bit] -> Int 
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
	where weights = iterate (*2) 1

int2bin :: Int -> [Bit] 
int2bin 0 = [] 
int2bin n = n `mod` 2 : int2bin (n `div` 2) 

make8 :: [Bit] -> [Bit] 
make8 bits = take 8 (bits ++ repeat 0) 

encode :: String -> [Bit] 
encode = concat . (map (parity . make8 . int2bin . ord))

parity :: [Bit] -> [Bit]
parity x = if odd (sum x) then 1 : x else 0 : x 
		 

--before map, add (if count of ones is odd then 1 else 0) : map something or other 
chop9 :: [Bit] -> [[Bit]]
chop9 [] = [] 
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String 
decode = map (chr . bin2int . tail) . chop9 

transmit :: String -> String 
transmit = decode . channel . encode 

channel :: [Bit] -> [Bit] 
channel = id 

