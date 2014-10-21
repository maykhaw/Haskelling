import Data.Char 

main = do
    putStrLn "What's your first name?" 
    firstName <- getLine 
    putStrLn "What's your last name?" 
    lastName <- getLine 
    let bigFirstname = map toUpper firstName
        bigLastname = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstname ++ ' ' ++ bigLastname ++ ", how are you?" 
