import System.Environment
import Data.Char
import Data.List.Split
import Text.PrettyPrint as P

path = "log.txt"

data T = T { date :: String
           , txt :: String }
    deriving Show


wrapd l = fmap (++ padding) $ l : repeat filler
    where
        padding = " | "
        filler = replicate (length l) ' '


wrapt n t = fmap (elim ' ') $ foldr (\x acc -> (wrapperize n x) ++ acc) [] t'
    where t' = filter (not . null) $ splitOn "\n" t


wrapperize n [] = []
wrapperize n s = (take x s) : wrapperize n (drop x s)
    where
        indices = indexOf ' ' s
        x = drpWhile n indices s

elim _ [] = []
elim c a@(x:xs)
    | x == c  = elim c xs
    | otherwise = a

drpWhile n [] s = length s
drpWhile n (x:xs) s
    | x < n     = drpWhile n xs s
    | otherwise = x


indexOf :: Char -> [Char] -> [Int]
indexOf x = map fst . filter (\(_,s) -> s==x) . zip [0..]

print_every T{ date = s_date, txt = s_text} = mapM_ putStrLn $ zipWith (++) wdate wtext
    where
        wtext = wrapt 55 s_text
        wdate = wrapd s_date


make_record s = do
    let s_date = drop 1 $ init $ take 12 s
        s_text = drop 1 $ drop 12 s
    T s_date s_text


main = do
    content <- readFile path
    let newc = splitOn "#########" content
        -- mdata = mapM_ print_every newc
        mdata = fmap make_record newc
    mapM_ print_every $ take 4 mdata


