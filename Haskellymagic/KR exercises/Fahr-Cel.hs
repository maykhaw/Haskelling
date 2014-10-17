
-- print a Fahr - Cels table for 0-300 with step 20 

celsius fahr = round $ (5 / 9) * (fahr -32) 
tabledata = zip [0, 20..300] $ map celsius [0, 20..300] 

tuple2str :: (Int, Int) -> String 
tuple2str (fahr, cels) = show fahr ++ "\t" ++ show cels ++ "\n" 

listuple2str :: [(Int, Int)] -> String 
listuple2str l = concat $ map tuple2str l 

main = do 
	putStrLn "Fahrenheit to Celsius Table" 
	putStr $ listuple2str tabledata 
