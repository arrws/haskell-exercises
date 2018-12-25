
import Data.List
import Data.Function


data MTree a = MNode a [MTree a]
    deriving (Eq, Show)

string_to_mtree :: String -> MTree Char
string_to_mtree (x:xs) = MNode x tree
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



data Adjacencies a = Adj [(a, [a])]
           deriving (Show, Eq)

data Edges a = Edge [(a, a)]
          deriving (Show, Eq)


adj_to_friends :: (Eq a) => Adjacencies a -> Edges a
adj_to_friends (Adj []) = Edge []
adj_to_friends (Adj ((x,y):xs)) = Edge $ foldr (\a b -> (x, a) : b) [] y ++ rest
                                where Edge rest = adj_to_friends $ Adj xs

adj_to_friends' :: (Eq a) => Adjacencies a -> Edges a
adj_to_friends' (Adj x) = Edge $ go x
                        where
                        go [] = []
                        go ((x,y):xs) = foldr (\a b -> (x, a) : b) [] y ++ go xs

friends_to_adj :: Edges Char -> Adjacencies Char
friends_to_adj (Edge f) = Adj $ map (transform) $ groupBy (\(x,_) (y,_) -> x == y) f
                        where
                        transform a@((x,_):_) = (x, foldr (\x y -> (snd x) : y) "" a)


get_paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
get_paths x y edges
            | x == y    = [[y]]
            | otherwise = [ x:path | edge <- edges, (fst edge) == x,
                                     path <- (get_paths (snd edge) y [e | e<-edges, e/=edge])
                          ]


find_cycle :: Int -> [(Int, Int)] -> [ [Int]  ]
find_cycle n g = go n n g 0
    where go x y edges ok
            | x == y && ok == 1 = [[y]]
            | otherwise         = [ x:path | edge <- edges, (fst edge) == x,
                                             path <- (go (snd edge) y [e | e<-edges, e/=edge && (fst e)/=n] 1)
                                  ]



queens :: Int -> [[Int]]
queens n = map reverse $ go n
        where
        go 0    = [[]]
        go k    = [q:qs | qs <- go (k-1), q <- [1..n], not (q `elem` qs || check_diag q qs)]
        check_diag x qs = any (\(diag, q) -> abs(x-q) == diag) $ zip [1..] qs


automata :: String -> Bool
automata s = go s 'l'
    where
    go []    t = t == 'e'
    go (c:s) t = (match c t) && or [go s b | (a, b) <- lst, a == t]
    lst = [('l', 'e'), ('l', 'l'), ('l', '-'), ('l', 'd'), ('-', 'l'), ('-', 'd'), ('d', 'e'), ('d', '-'), ('d', 'l'), ('d', 'd')]
    match c t
            | t == '-' = c == '-'
            | t == 'd' = '0' <= c && c <= '9'
            | t == 'l' = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'
            | otherwise = False


main = do
    let multi_tree = string_to_mtree "afg^^c^bd^e^^^"
    print( multi_tree )
    print( sum_paths multi_tree )
    print( bottom_up multi_tree )
    print( lisp_print multi_tree )
    let test = Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
    print( test )
    print( adj_to_friends test )
    print( adj_to_friends' test )
    print( friends_to_adj $ adj_to_friends test )
    let graph = [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print( get_paths 1 4 graph )
    print( find_cycle 2 graph )
    print( queens 6 )
    print( automata "iden-tifi-er200" )

