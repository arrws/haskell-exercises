double x = if x > 100
    then x
    else x*2

fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

tell :: (Show a) => [a] -> String
tell [] = "empty"
tell (x:[]) = "one elem " ++ show x
tell (x:y:[]) = "two elems " ++ show x ++ " " ++ show y
tell (x:y:_) = "many elems "

desc :: [a] -> String
desc xs = "list is " ++ case xs of [] -> "empty"
                                   [x] -> "single"
                                   xs -> "longer"

comp :: (Ord a, Num a) => a -> a -> a
comp x y
    | x<m && y>m    = m
    | m<x           = x
    | m>y           = y
    | otherwise     = 0
    where m = 18

bmi :: (RealFloat a) => [(a, a)] -> [a]
bmi xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 1.5]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = quicksort [a | a <- xs, a <= x]
        greater = quicksort [a | a <- xs, a > x]
    in  smaller ++ [x] ++ greater

apply :: (a -> a) -> a -> a
apply f x = f (f x)


main =
    print( double 5 ) >>
    print( [1..30] !! 6 ) >>
    print( ['a'..'z'] ) >>
    print( "Hello"++" world" ) >>
    print( [1]++[2,3,4] ) >>
    print( 1:[2,3,4] ) >>

    -- LIST FUNCTIONS
    -- head / last
    -- tail / init
    -- null ~ is empty?
    -- reverse
    -- maximum / minimum
    -- sum / product
    print( length [5,4,3] ) >>
    print( take 10 (cycle [1,2,3]) ) >>
    print( drop 3 [5,4,3,2,1] ) >>
    print( 4 `elem` [5,4,3] ) >>
    print( elem 4 [5,4,3] ) >>
    print( replicate 3 10 ) >>

    print( [x*2 | x <- [1..5]] ) >>
    print( [x*2 | x <- [1..5], x*2 >= 5, x /= 4] ) >>
    print( [ [x | x <- xs, even x] | xs <- [[1,2,3],[2,3,4],[4,5]] ] ) >>

    -- TYPECLASSES
    -- Int Integer
    -- Float Double
    -- Bool Char
    -- Tuples
    -- Enum Bounded Eq
    -- Num Integral Floating
    print( zip [1..3] ['a'..'c'] ) >>
    print( snd (9, "Hey") ) >>
    print( fst (8, 11) ) >>
    print( compare (fac 8) (fac 5) ) >>
    print( tell [1..3] ) >>
    print( tell [1] ) >>

    print( comp 1 2 ) >>
    print( comp 15 20 ) >>
    let x = 18
    in print( comp x x ) >>

    print( bmi [(420.5, 5.0), (550.0, 10.0)] ) >>
    print( desc [] ) >>
    print( desc [1..5] ) >>

    print( quicksort [42, 5, 15, 55, 10] ) >>
    print( (+3) 10  ) >>
    print( (3:) [1] ) >>

    print( zipWith (flip div) [2,2..] [2,6,2,3] ) >>
    print( map (replicate 3) [3..6] ) >>
    print( filter (>3) [1,5,2,3,6] ) >>

    print( sum (takeWhile (<1000) [n^2 | n <- [1..], odd (n^2)]) ) >>
    print( map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] ) >>
    print( foldl (\acc x -> acc + x) 0 [2,5,10,1,3,8] ) >>
    print( foldl (flip (:)) [] [2,5,10,1,3,8] ) >>

    print( map ($ 3) [(4+), (10*), (^2), sqrt] ) >>
    print( map (negate . abs) [5,-3,-6,7,-3,2,-19,24] ) >>
    print( replicate 5 . sum . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8] )

