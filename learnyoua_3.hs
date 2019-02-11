import Data.List
import Data.Set
import Data.Char
import Data.Map




main = do
        let s = "Monkeyman"
        print $ intersperse '-' s
        print $ intercalate "-" [s, s, s]
        print $ transpose [s, s, s]
        print $ Data.List.splitAt 3 s
        let a = [2,3,5,4,1,5,1,0,5,9,0,2]
        print $ Data.List.partition (>3) a
        print $ find (>3) a
        print $ group a
        print $ zipWith3 (\x y z -> x+y+z) a a a
        print $ nub a
        print $ Data.List.delete 'n' . Data.List.delete 'n' $ s
        print $ [1..10] Data.List.\\ [2,3,5,6,9]
        print $ Data.List.union "abecedar" "alfabet"
        let fck = [("a", 3), ("d", 1), ("k", 8), ("x", 4), ("b", 0)]
        -- print $ findKey "k" fck



