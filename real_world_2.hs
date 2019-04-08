import Data.Array
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>))
import Data.Foldable
import qualified Data.DList as DList


nums = ["101", "110", "001", "010", "0", "11010100110"]

nums2 = map complement <$> nums
    where
        complement '0' = '1'
        complement '1' = '0'

list_to_array :: [a] -> Array Int a
list_to_array x = listArray(0, (length x) -1) x


foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
            where
                go s (j:js) = let s' = f s (a!j)
                                in s' `seq` go s' js
                go s _ = s

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a


main = do
        let x = list_to_array nums
        print $ x ! 3
        print $ list_to_array nums2
        let y = listArray ('a', 'v') [101..]
        print $ y ! 'f'
        print $ listArray (-20, 0) ['a'..]
        let z = listArray ((0,0,0), (9,9,9)) [0..]
        print $ z ! (6,3,4)

        print $ foldA (+) 0 (list_to_array [0..9])
        print $ foldA1 (+) (list_to_array [0..9])
        let q = [(1, "one"), (2, "zero"), (3, "six"), (4, "four"), (5, "three")]
        print $ lookup 1 q
        print $ lookup 6 q

        let m = Map.fromList q
        print $ m
        let mm = Map.insert 2 "two" .
                    Map.insert 1 "unu" .
                    Map.insert 0 "zero" .
                    Map.insert 3 "three"
                    $ Map.empty
        print $ mm
        print $ toList mm

        print $ Seq.singleton 1
        print $ 1 <| 2 <| 3 <| Seq.singleton 4
        print $ Seq.singleton 1 |> 2 |> 3 |> 4
        let l = Seq.fromList [1,2,3]
            r = Seq.fromList [5,6,8]
        print $ l >< r
        print $ toList (Seq.fromList [1..5])

