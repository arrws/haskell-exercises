
data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)


gen_balanced_trees :: Int -> [Tree Char]
gen_balanced_trees 0 = [Empty]
gen_balanced_trees n = let (q, r) = (n-1) `quotRem` 2
    in [Branch 'x' left right | i <- [q .. q+r],
                               left  <- gen_balanced_trees i,
                               right <- gen_balanced_trees (n-i-1)]

gen_balanced_trees' :: Int -> [Tree Char]
gen_balanced_trees' 0 = [Empty]
gen_balanced_trees' 1 = [Branch 'x' Empty Empty] -- leaf
gen_balanced_trees' n = if n `mod` 2 == 1 then
                        [Branch 'x' l r | l <- gen_balanced_trees' ((n-1) `div` 2),
                                          r <- gen_balanced_trees' ((n-1) `div` 2) ]
                        else
                        concat [ [Branch 'x' l r, Branch 'x' r l] |
                                          l <- gen_balanced_trees' ((n-1) `div` 2),
                                          r <- gen_balanced_trees' (n `div` 2) ]

gen_sym_balanced_trees :: Int -> [Tree Char]
gen_sym_balanced_trees n = if n `mod` 2 == 0 then [] else
                           [Branch 'x' t (reverse t) | t <- gen_balanced_trees (n `div` 2)]
                where reverse Empty = Empty
                      reverse (Branch x l r) = Branch x (reverse r) (reverse l)

gen_sym_balanced_trees' = filter is_symetric . gen_balanced_trees


is_mirror :: Tree Char -> Tree Char -> Bool
is_mirror Empty Empty = True
is_mirror (Branch _ x y) (Branch _ a b) = is_mirror x b && is_mirror y a
is_mirror _ _     = False

is_symetric :: Tree Char -> Bool
is_symetric t = is_mirror t t


add :: Ord a => a -> Tree a -> Tree a
add x Empty             = Branch x Empty Empty
add x t@(Branch y l r)  = case compare x y of
                           LT -> Branch y (add x l) r
                           GT -> Branch y l (add x r)
                           EQ -> t

build_tree :: [Int] -> Tree Int
build_tree n = foldl (flip add) Empty n


main = do
    print( gen_balanced_trees 4 )
    print( gen_balanced_trees' 4 )
    print( gen_sym_balanced_trees 5 )
    print( gen_sym_balanced_trees' 5 )
    print( is_symetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) )
    print( is_symetric (Branch 'x' Empty (Branch 'x' Empty Empty)) )
    print( build_tree [3, 2, 5, 7, 1] )
