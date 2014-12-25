{-# LANGUAGE ScopedTypeVariables #-} 
module Main where 
import System.Environment 

main :: IO () 
main = do
    (args :: [String])  <- getArgs
    let newargs :: [Int] 
        newargs = map read args 
    putStrLn . show $ (newargs !! 0 + newargs !! 1) 
