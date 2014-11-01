import qualified Data.Set as Set
import Test.QuickCheck


-- Given a list of constraints (x,y) that mean
-- x has to be done before y,
-- figure out an overall ordering for our tasks.
-- Or, if that's not possible, return a sublist of
-- elements that has circular dependency.
tsort :: Ord a => [(a,a)] -> Either [a] [a]
tsort l = _

-- All elements are in there after we sort.
testAll :: [(Int,Int)] -> Bool
testAll list = let (l,r) = unzip list
                   allElements = Set.fromList $ l ++ r in
    case tsort list of
        Right order -> Set.fromList order == allElements
        Left circle -> Set.fromList circle `Set.isSubsetOf` allElements

loopedPairs :: [a] -> [(a,a)]
loopedPairs list = zip list (tail list ++ list)

testOrder :: [(Int, Int)] -> Bool
testOrder constraints = case tsort constraints of
    Left circle -> all (`elem` constraints) $ loopedPairs circle
    Right order -> all satisfied constraints where
        satisfied (a,b) = filter (`elem` [a,b]) order == [a,b]

main = do
    quickCheck testAll
    quickCheck testOrder
