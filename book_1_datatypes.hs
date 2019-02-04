module Main where

import Data.Char
import Data.Time


-- CYPHER

encode :: Char -> Int
encode ch = ord ch - ord 'a'

decode :: Int -> Char
decode n = chr (ord 'a' + n)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = decode $ mod (f (encode ch) n) 26

caesar :: Int -> String -> String
caesar n = map (shift (+) n)

decaesar :: Int -> String -> String
decaesar n = map (shift (-) n)

vigenere :: String -> String -> String
vigenere w = zipWith (shift (+) . encode) (concat $ repeat w)

unVigenere :: String -> String -> String
unVigenere w = zipWith (shift (-) . encode) (concat $ repeat w)



-- DATABASE

data DatabaseItem
  = DBString String
  | DBNumber Integer
  | DBDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DBDate (UTCTime (fromGregorian 1995 5 1) (secondsToDiffTime 123))
  , DBNumber 9001
  , DBString "Hello world!"
  , DBDate (UTCTime (fromGregorian 1997 5 1) (secondsToDiffTime 123))
  , DBNumber 66
  , DBString "test"
  ]

filterDBDate :: [DatabaseItem] -> [UTCTime]
filterDBDate db = [t | (DBDate t) <- db]

filterDBNumber :: [DatabaseItem] -> [Integer]
filterDBNumber db = [n | (DBNumber n) <- db]

avgDB :: [DatabaseItem] -> Double
avgDB db =
    let nums = filterDBNumber db in
        if length nums == 0
            then 0
            else (fromIntegral (sum nums)) / (fromIntegral (length nums))



-- BINTREE

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  postorder left ++ postorder right ++ [a]


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) =
  foldTree f (foldTree f (f a b) left) right -- preorder
  -- foldTree f (f a (foldTree f b left)) right    -- inorder
  -- f a (foldTree f (foldTree f b left) right) -- postorder



is_subseq :: Eq a => [a] -> [a] -> Bool
is_subseq [] _ = True
is_subseq _ [] = False
is_subseq s@(a:as) (b:bs) = (a == b && is_subseq as bs) || is_subseq s bs

main = do
    print(vigenere "ally" "meetatdawn")     -- => "mppraeoywy"
    print(unVigenere "ally" "mppraeoywy")   -- => "meetatdawn"
    print (is_subseq "var" "radvaru")
    print (is_subseq "var" "radyaru")


