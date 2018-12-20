import Data.List
import Data.Ord (comparing)
import Control.Monad (replicateM)

is_prime :: Int -> Bool
is_prime x
    | x == 2    = True
    | x < 3     = False
    | otherwise = foldr (&&) True [(x `mod` i) /= 0 | i <- 2:[3,5..s]]
        where s = floor $ sqrt $ fromIntegral x

get_gcd :: Int -> Int -> Int
get_gcd x 0 = abs x
get_gcd x y = get_gcd y (mod x y)

euler_totient :: Int -> Int
euler_totient x = foldr (+) 0 [if (get_gcd x i) == 1 then 1 else 0 | i <- [1,2..x-1]]


prime_factors :: Int -> [Int]
prime_factors x = factorize x $ 2:[3,5..s]
        where s = floor $ sqrt $ fromIntegral x
              test x y = is_prime y && x `mod` y == 0
              factorize 1 [] = []
              factorize x a@(y:ys)
                  | test x y    = y : factorize (x `div` y) a
                  | otherwise   = factorize x ys

prime_factors' = map encode . group . prime_factors
        where encode x = (head x, length x)


golbach :: Int -> (Int, Int)
golbach n = head [(x,y) | x <- primes, let y = n-x, is_prime y]
       where primes = filter (is_prime) $ 2:[3,5..s]
             s = floor $ sqrt $ fromIntegral n



and'  a b = a && b
or'   a b = a || b
nand' a b = not (and' a b)
nor'  a b = not (or' a b)
xor'  a b = not (equ' a b)
impl' a b = or' (not a) b
equ'  a b = a == b

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

truth_table :: Int -> ([Bool] -> Bool) -> IO ()
truth_table n f = mapM_ putStrLn [toStr a ++ " -> " ++ translate (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> translate x )
          translate x = if x then "T" else "F"



gray_code :: Int -> [String]
gray_code 0 = [""]
gray_code n = let codes = gray_code (n-1) in
            ["0"++x | x <- codes] ++ ["1"++x | x <- codes]

gray_code' :: Int -> [String]
gray_code' 0 = [""]
gray_code' n = foldr (\x acc -> ("0"++x):("1"++x):acc) [] $ gray_code' $ n-1



huffman :: [(Char, Int)] -> [(Char, String)]
huffman x = format $ combine $ resort $ morph x
    where
        morph x = [ ([[]], [c], n) | (c,n) <- x ]
        resort x = sortBy (\(_,_,a) (_,_,b) -> compare a b) x
        format (x,y,_) = sortBy (\(a,b) (x,y) -> compare (length b) (length y)) $ zip y x
        combine (x:[]) = x
        combine (x:y:xs) = combine $ resort $ (combi x y) : xs
            where combi(a,b,c) (x,y,z) = ((map ('0':) a) ++ (map ('1':) x), b++y, c+z)



main = do
    print( is_prime 7 )
    print( get_gcd 36 63 )
    print( euler_totient 10 )
    print( prime_factors 315 )
    print( prime_factors' 315 )
    print( golbach 28 )
    truth_table 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
    print( gray_code 3 )
    print( gray_code' 3 )
    print( huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] )


