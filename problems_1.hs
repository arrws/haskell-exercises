
last_elem = head . reverse

sec_last_elem = head . tail . reverse
sec_last_elem' = last . init


kth_elem :: [a] -> Int -> a
kth_elem (x:_) 1 = x
kth_elem (_:xs) a = kth_elem xs (a-1)
kth_elem _ _ = error "fuck"

kth_elem' :: [a] -> Int -> a
kth_elem' l 1 = head l
kth_elem' l a = kth_elem (tail l) (a-1)

kth_elem'' x n
  | length x<n = error "fuck"
  | otherwise = last $ take n x


num_elems :: [a] -> Int
num_elems [] = 0
num_elems (_:xs) = 1 + num_elems xs

num_elems' :: [a] -> Int
num_elems' l = num_elems_helper l 0
    where
        num_elems_helper [] k = k
        num_elems_helper (_:xs) k = num_elems_helper xs (k+1)

num_elems'' :: [a] -> Int
num_elems'' = sum.map(\_ -> 1)

num_elems''' :: [a] -> Int
num_elems''' = foldr(\_ -> (+1)) 0


reverse_elems :: [a] -> [a]
reverse_elems [] = []
reverse_elems (x:xs) = (reverse_elems xs) ++ [x]

reverse_elems' :: [a] -> [a]
reverse_elems' = foldl(\a x -> x:a) []


is_palindrom :: (Eq a) => [a] -> Bool
is_palindrom [] = True
is_palindrom [_] = True
is_palindrom (x:xs) = (x == (last xs)) && is_palindrom (init xs)

is_palindrom' :: (Eq a) => [a] -> Bool
is_palindrom' x = foldr (&&) True $ zipWith(==) x (reverse x)

data NestedList a = Elem a | List [NestedList a]

flatten_list :: NestedList a -> [a]
flatten_list (Elem x) = [x]
flatten_list (List x) = concatMap flatten_list x

flatten_list' :: NestedList a -> [a]
flatten_list' (Elem x) = [x]
flatten_list' (List (x:xs)) = flatten_list'(x) ++ flatten_list'(List xs)
flatten_list' (List []) = []


del_consec_dupes :: Eq a => [a] -> [a]
del_consec_dupes [x] = [x]
del_consec_dupes (x:xs) = if x==(head xs) then del_consec_dupes xs else  [x] ++ del_consec_dupes xs

-- del_consec_dupes' :: Eq a => [a] -> [a]
-- del_consec_dupes' = map head . group

del_consec_dupes'' :: Eq a => [a] -> [a]
del_consec_dupes'' [] = []
del_consec_dupes'' (x:xs) = x:(del_consec_dupes'' $ dropWhile (==x) xs)


pack_consec_dupes :: Eq a => [a] -> [[a]]
pack_consec_dupes [] = []
pack_consec_dupes (x:xs) = (x : (filter (==x) xs)):(pack_consec_dupes $ filter(/=x) xs)


encode_list :: Eq a => [a] -> [(Int, a)]
encode_list [] = []
encode_list (x:xs) = ((1 + length (filter (==x) xs)), x) : (encode_list $ filter(/=x) xs)

-- encode_list' :: Eq a => [a] -> [(Int, a)]
-- encode_list' l = map(\x -> (length x, head x)) (group l)

-- encode_list'' :: Eq a => [a] -> [(Int, a)]
-- encode_list'' l = [(length x, head x) | x <- group l]


data Encoding a = Single a | Multiple Int a
    deriving (Show)

new_encode_list :: (Eq a, Show a) => [a] -> [Encoding a]
new_encode_list = map encodeHelper . encode_list
    where
        encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

-- new_encode_list :: (Eq a, Show a) => [a] -> [Encoding a]
-- new_encode_list [] = []
-- new_encode_list (x:xs) = let y = filter(/=x) xs in
--                          let k = (1 + length (filter (==x) xs)) in
--                          let z = if x /= (head xs) then Single x else Multiple k x in
--                          z:[] --(new_encode_list y)


decode_list :: Eq a => [Encoding a] -> [a]
decode_list = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x


direct_encode_list :: (Eq a, Show a) => [a] -> [Encoding a]
direct_encode_list [] = []
direct_encode_list (x:xs) = encodeHelper 1 x xs
encodeHelper n y [] = [encodeElement n y]
encodeHelper n y (x:xs) | y==x  = encodeHelper (n+1) y xs
                      | otherwise = encodeElement n y : (encodeHelper 1 x xs)
encodeElement 1 y = Single y
encodeElement n y = Multiple n y


main = do
    print( head [1,2,3,4,5,6] )
    print( tail [1,2,3,4,5,6] )
    print( last [1,2,3,4,5,6] )
    print( init [1,2,3,4,5,6] )

    print( kth_elem [1,2,3,4,5,6] 3 )
    print( [1,2,3,4,5,6] !! 3 ) -- infix

    print( take 2 [1,2,3,4,5,6] )
    print( drop 2 [1,2,3,4,5,6] )
    print( num_elems [1,2,3,4,5,6] )
    print( reverse_elems [1,2,3,4,5,6] )

    print( is_palindrom [1,2,3,2,1] )
    print( num_elems [1,2,3,4,5,6] )
    print( flatten_list (List [Elem 1,List [Elem 2,List [Elem 3]],Elem 4,List [Elem 5,Elem 6]]))
    print( del_consec_dupes [1,1,1,2,5,5,8] )
    print( pack_consec_dupes [1,1,1,2,5,5,8] )
    print( encode_list [1,1,1,2,5,5,8] )
    print( new_encode_list [1,1,1,2,5,5,8] )
    print( decode_list [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'])
    print( direct_encode_list [1,1,1,2,5,5,8] )



