
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)


gen_balanced_trees :: Int -> [Tree Char]
gen_balanced_trees 0 = [Empty]
gen_balanced_trees n = let (q, r) = (n-1) `quotRem` 2
    in [Node 'x' left right | i <- [q .. q+r],
                               left  <- gen_balanced_trees i,
                               right <- gen_balanced_trees (n-i-1)]

gen_balanced_trees' :: Int -> [Tree Char]
gen_balanced_trees' 0 = [Empty]
gen_balanced_trees' 1 = [Node 'x' Empty Empty] -- leaf
gen_balanced_trees' n = if n `mod` 2 == 1 then
                        [Node 'x' l r | l <- gen_balanced_trees' ((n-1) `div` 2),
                                          r <- gen_balanced_trees' ((n-1) `div` 2) ]
                        else
                        concat [ [Node 'x' l r, Node 'x' r l] |
                                          l <- gen_balanced_trees' ((n-1) `div` 2),
                                          r <- gen_balanced_trees' (n `div` 2) ]

gen_sym_balanced_trees :: Int -> [Tree Char]
gen_sym_balanced_trees n = if n `mod` 2 == 0 then [] else
                           [Node 'x' t (reverse t) | t <- gen_balanced_trees (n `div` 2)]
                where reverse Empty = Empty
                      reverse (Node x l r) = Node x (reverse r) (reverse l)

gen_sym_balanced_trees' = filter is_symetric . gen_balanced_trees


gen_height_trees :: Int -> [Tree Char]
gen_height_trees 0 = [Empty]
gen_height_trees 1 = [Node 'x' Empty Empty]
gen_height_trees h = [Node 'x' l r |
                    (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
                    l <- gen_height_trees hl,
                    r <- gen_height_trees hr]


is_mirror :: Tree Char -> Tree Char -> Bool
is_mirror Empty Empty = True
is_mirror (Node _ x y) (Node _ a b) = is_mirror x b && is_mirror y a
is_mirror _ _     = False

is_symetric :: Tree Char -> Bool
is_symetric t = is_mirror t t


add :: Ord a => a -> Tree a -> Tree a
add x Empty             = Node x Empty Empty
add x t@(Node y l r)  = case compare x y of
                           LT -> Node y (add x l) r
                           GT -> Node y l (add x r)
                           EQ -> t

build_tree :: [Int] -> Tree Int
build_tree n = foldl (flip add) Empty n


count_leaves :: Tree a -> Int
count_leaves Empty = 0
count_leaves (Node _ Empty Empty) = 1
count_leaves (Node _ x y) = count_leaves x + count_leaves y

get_leaves :: Tree a -> [a]
get_leaves Empty                    = []
get_leaves (Node x Empty Empty)   = [x]
get_leaves (Node _ x y)           = get_leaves x ++ get_leaves y

get_nodes :: Tree a -> [a]
get_nodes Empty                     = []
get_nodes (Node _ Empty Empty)    = []
get_nodes (Node x l r)            = x : get_nodes l ++ get_nodes r

get_level :: Tree a -> Int -> [a]
get_level Empty _           = []
get_level (Node x _ _) 1  = [x]
get_level (Node x l r) h  = get_level l (h-1) ++ get_level r (h-1)


build_complete_tree :: Int -> Tree Char
build_complete_tree 0 = Empty
build_complete_tree 1 = Node 'x' Empty Empty
build_complete_tree h = Node 'x' l r
                        where
                        l = build_complete_tree (h `div` 2)
                        r = build_complete_tree ((h-1) `div` 2)

pretty_print :: Tree Int -> String
pretty_print t  = go t 0
    where
    tab h = replicate h '\t'
    go Empty h                  = tab h ++ "Empty\n"
    go (Node x Empty Empty) h = tab h ++ "Node " ++ show x ++ " (Empty Empty)\n"
    go (Node x l r) h         = tab h ++ "Node " ++ show x ++ " (\n" ++ go l (h+1) ++ go r (h+1) ++ tab (h+1) ++ ")\n"


treeToString :: Tree Char -> String
treeToString Empty                  = ""
treeToString (Node x Empty Empty) = [x]
treeToString (Node x l r)         = x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"

stringToTree :: String -> Tree Char
stringToTree s = tree
    where
    (_, tree) = go s
    go s@(x:xs) | x == ',' || x == ')' = (s, Empty)
    go (x:y:xs) | y == ',' || y == ')' = (y:xs, Node x Empty Empty)
                | y == '('             = (xs'', Node x l r)
                                       where
                                       (',':xs', l)  = go xs
                                       (')':xs'', r) = go xs'



data MTree a = MNode a [MTree a]
    deriving (Eq, Show)

stringToMTree :: String -> MTree Char
stringToMTree (x:xs) = MNode x tree
    where
    (tree, _) = go xs
    go (x:xs) | x == '^'  = ([], xs)
              | otherwise = ([MNode x next_node] ++ next_next_node, rest_final)
                          where
                          (next_node, rest) = go xs
                          (next_next_node, rest_final) = go rest

sum_paths :: MTree Char -> Int
sum_paths t = go 0 t
            where
            go p (MNode x t)  = p + foldl (\x y -> x + go (p+1) y) 0 t
            -- go p (Node x t)  = p + sum (map (go (p+1)) t)

bottom_up :: MTree Char -> String
bottom_up (MNode x t) = foldl (\x y -> x ++ (bottom_up y) ) "" t ++ [x]

lisp_print :: MTree Char -> String
lisp_print (MNode x []) = " " ++ [x]
lisp_print (MNode x t) = " (" ++ [x] ++ foldl (\x y -> x ++ (lisp_print y) ) "" t ++ ")"



main = do
    print( gen_balanced_trees 4 )
    print( gen_balanced_trees' 4 )
    print( gen_height_trees 2 )
    print( gen_sym_balanced_trees 5 )
    print( gen_sym_balanced_trees' 5 )
    print( is_symetric (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty)) )
    print( is_symetric (Node 'x' Empty (Node 'x' Empty Empty)) )
    print( build_tree [3, 2, 5, 7, 1] )
    let test_tree = (Node 1 (Node 2 Empty (Node 4 Empty Empty)) (Node 5 Empty Empty))
    print( test_tree )
    print( count_leaves test_tree )
    print( get_leaves test_tree )
    print( get_nodes test_tree )
    print( get_level test_tree 2 )
    print( build_complete_tree 4 )
    putStrLn( pretty_print (Node 0 test_tree test_tree) )
    let str_tree = "x(y,a(,b))"
    print( stringToTree str_tree )
    print( treeToString (stringToTree str_tree) == str_tree )
    let multi_tree = stringToMTree "afg^^c^bd^e^^^"
    print( multi_tree )
    print( sum_paths multi_tree )
    print( bottom_up multi_tree )
    print( lisp_print multi_tree )


