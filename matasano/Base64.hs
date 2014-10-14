import Data.Char
import Data.ByteString

main = putStr . filter (not . isAlphaNum) =<< getContents


