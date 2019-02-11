import Data.List
import Data.Set
import Data.Char
import Data.Map
import Control.Monad
import Control.Monad.Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
            | x < 4 = do
                    tell ["keeping " ++ show x]
                    return True
            | otherwise = do
                    tell [show x ++ " is too large, throwing it away "]
                    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs


-- FOCUS list
type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)


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

        -- print $ liftM (*3) (Just 8)
        -- print $ runWriter $ fmap not $ Writer (True, "chickpeas")
        -- print $ join Just ( Just ( Just 5 ) )
        -- mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
        -- print $ powerset [1, 2, 3]

        let xs = [1,2,3,4]
        print $ goForward (xs,[])
        print $ goForward ([2,3,4],[1])
        print $ goForward ([3,4],[2,1])
        print $ goBack ([4],[3,2,1])

