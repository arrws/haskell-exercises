import Data.Array

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)


fib :: [Int]
fib = 1 : 1 : [x+y | (x,y) <- zip fib (tail fib)]

fib' :: [Int]
fib' = 1 : 1 : map (\(x,y) -> x+y) (zip fib (tail fib))

fib'' :: [Int]
fib'' = 1 : 1 : zipWith (+) fib (tail fib)


take_this :: Int -> [a] -> [a]
take_this 0     _      = []
take_this _     []     = []
take_this n (x:xs) = x:take_this (n-1) xs

take_this' :: Int -> [a] -> [a]
take_this' _     []     = []
take_this' 0     _      = []
take_this' n (x:xs) = x:take_this' (n-1) xs


type Response = Int
type Request = Int

client :: Request -> [Response] -> [Request]
client init ~(resp:resps) = init : client (next resp) resps

server :: [Request] -> [Response]
server (req : reqs) = process req : server reqs

-- maps the response from the previous request onto the next request
next :: Response -> Request
next resp = resp

-- maps a request to a response
process :: Request -> Response
process req = req+1

requests :: [Request]
requests = client 0 responses

responses :: [Response]
responses = server requests


class Eq' a where
    eq :: a -> a -> Bool

instance Eq' Int where
    x `eq` y = x == y

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

instance (Eq' a) => Eq' (Tree a) where
  (Leaf a)       `eq` (Leaf b)       = a `eq` b
  (Branch l1 r1) `eq` (Branch l2 r2) =  (l1 `eq` l2) && (r1 `eq` r2)
  _              `eq` _              = False

showsTree  :: Show a => Tree a -> String -> String
showsTree (Leaf x) s = shows x s
showsTree (Branch l r) s = '<' : showsTree l ('|' : showsTree r ('>' : s))

readsTree :: (Read a) => ReadS (Tree a)
readsTree ('<':s)  = [(Branch l r, u) | (l, '|':t) <- readsTree s,
                                        (r, '>':u) <- readsTree t ]
readsTree s        = [(Leaf x,t)      | (x,t) <- reads s]


data Point = Pt {pointx, pointy :: Float} deriving Show


e1 = [(x,y) | x <- [1,2,3] , y <- [1,2,3], x /= y]

e2 = do x <- [1,2,3]
        y <- [1,2,3]
        True <- return (x /= y)
        return (x,y)

e3 = [1,2,3] >>= (\ x -> [1,2,3] >>= (\y -> return (x/=y) >>=
     (\r -> case r of True -> return (x,y)
                      _    -> fail "")))

mvLift2 :: (a -> b -> c) -> [a] -> [b] -> [c]
mvLift2 f x y = do x' <- x
                   y' <- y
                   return (f x' y')

e4 = mvLift2 (+) [1,3] [10,20,30]
e5 = mvLift2 (\a b -> [a,b]) "ab" "cd"
e6 = mvLift2 (*) [1,2,4] []


atake :: (Ix a) => Array a b -> (a,a) -> Array a b
atake a (l,u) | inRange (bounds a) l && inRange (bounds a) u =
                   array (l,u) [(i,a!i) | i <- range (l,u)]
              | otherwise = error "Subarray out of range"

wavefront :: Int -> Array (Int,Int) Int
wavefront n = a where
                a = array ((1,1),(n,n))
                     ([((1,j),1) | j <- [1..n]] ++
                      [((i,1),1) | i <- [2..n]] ++
                      [((i,j),a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j))
                                 | i <- [2..n], j <- [2..n]])


main = do
        print $ take 10 fib
        print $ take 10 fib'
        print $ take 10 fib''
        print ( take_this 0 (error "e1") :: [Int] )
        -- print ( take_this' 0 (error "e2") :: [Int] ) -- error
        -- print ( take_this (error "e3") [] :: [Int] ) -- error
        print ( take_this' (error "e4") [] :: [Int] )
        print $ take 5 responses
        let t1 = Branch (Leaf 1) (Leaf 2) :: Tree Int
        let t2 = Branch (Leaf 1) (Leaf 3) :: Tree Int
        print $ t1 `eq` t2
        print $ t1 `eq` t1
        let t3 = readsTree "<1|<2|3>>" :: [(Tree Int,String)]
        print $ t3
        print $ showsTree t2 ""
        let p1 = Pt {pointx = 1, pointy = 2}
        print $ p1
        let p2 = p1 {pointy = 0}
        print $ p2
        print $ pointy p2
        print $ e1
        print $ e2
        print $ e3
        print $ e4
        print $ e5
        print $ e6
        let wave = wavefront 6
        print $ wave
        print $ atake wave ((3,3),(5,5))
        -- print $ fmap (\x -> 0) wave
        -- print $ atake wave ((100,3),(5,5)) -- error

