-- ://www.interviewcake.com/question/word-cloud 

import Data.Char
import Data.List

lowerWords :: String -> [String] 
lowerWords [] = []
lowerWords l = sort $ words $ map toLower l 

dictionary :: [String] -> [String] 
dictionary [] = []  
dictionary [x] = [x] 
dictionary (x : y : xs) = if x == y then dictionary (x : xs) else x : dictionary (y : xs) 

wordcount :: [String] -> [(String, Int)] 
wordcount [] = []
wordcount (x : xs) = case wordcount xs of 
    [] -> [(x, 1)] 
    (y,n) : ys -> if y == x then (y, n + 1) : ys else (x, 1): (y, n) : ys  

wordcloud :: String -> [(String, Int)] 
wordcloud = wordcount . lowerWords
