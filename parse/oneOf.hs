

oneOf :: [Char] -> Parser Char  
oneOf l = Parser \char -> if char `elem` l then 
