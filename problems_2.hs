-- import System.Random
import List
import Data.Ord (comparing)


duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x: (duplicate xs)

duplicate' :: [a] -> [a]
duplicate' = concatMap (\x -> [x,x])

duplicate'' xs = xs >>= (\x -> [x,x])
duplicate''' l = concat [ [x,x] | x <- l ]


replicate' :: [a] -> Int -> [a]
replicate' xs n = foldl (\acc e -> acc ++ repl e n) [] xs
    where
        repl _ 0 = []
        repl x n = x : repl x (n-1)

replicate'' :: [a] -> Int -> [a]
replicate'' xs n = concatMap (replicate n) xs


drop_nth :: [a] -> Int -> [a]
drop_nth xs n = [i | (i, c) <- (zip xs [1,2..]), (mod c n) /= 0]

drop_nth' :: [a] -> Int -> [a]
drop_nth' xs n
  | length xs < n   = xs
  | otherwise       = take (n-1) xs ++ drop_nth' (drop n xs) n


split_list :: [a] -> Int -> ([a], [a])
split_list xs n
  | n >= 0 && n <= length xs     = (take n xs, drop n xs)


extract_list :: [a] -> Int -> Int -> [a]
extract_list xs n m
  | n >= 0 && m <= length xs     = take (m-n+1) $ drop (n-1) xs

extract_list' :: [a] -> Int -> Int -> [a]
extract_list' xs n m = [x | (x, i) <- zip xs [1..m], i >= n]


rotate_list :: [a] -> Int -> [a]
rotate_list xs n = let z = (mod n (length xs)) in
                      drop z xs ++ take z xs

remove_kth :: [a] -> Int -> [a]
remove_kth xs n = take (n-1) xs ++ drop n xs



insert_at :: a -> [a] -> Int -> [a]
insert_at x l n = take n l ++ [x] ++ drop n l

insert_at' :: a -> [a] -> Int -> [a]
insert_at' x l 0 = x:l
insert_at' x (l:ls) n = l: insert_at' x ls (n-1)

make_range :: Int -> Int -> [Int]
make_range x y
            | x < y     = x: make_range (x+1) y
            | otherwise = []

random_extract :: [a] -> Int -> IO[a]
random_extract = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

random_select :: Int -> Int -> IO[Int]
random_select n t = random_extract [1..t] n

random_perm :: [a] -> IO[a]
random_perm xs = random_extract xs (length xs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

sort_length :: [[a]] -> [[a]]
sort_length = sortBy (comparing length)
-- sort_length = sortBy (\xs ys -> compare (length xs) (length ys))





main =
    print( duplicate [1,2,3,4,5,6] ) >>
    print( replicate'' [1,2,3,4,5,6] 3) >>
    print( drop_nth [1,2,3,4,5,6] 3) >>
    print( split_list [1,2,3,4,5,6] 2) >>
    print( extract_list [1,2,3,4,5,6] 2 4) >>
    print( rotate_list [1,2,3,4,5,6] 4) >>
    print( remove_kth [1,2,3,4,5,6] 4) >>

    print( insert_at 777 [1,2,3,4,5,6] 3 ) >>
    print( make_range 3 7 ) >>
    print( sort_length [[1,2], [4], [3,4,5], [3,2]] ) >>

    print( )



