import Control.Concurrent
main = do
  loop 5 base

base = [[if r+c >= 33 && c-r <= 31 then '1' else '_' | c <- [1..63]] | r <- [1..32]]

dropEven s = map fst $ filter snd $ zip s $ cycle [False, True]

iter s = top ++ bot where
         half = map dropEven $ dropEven s
         padding = replicate 16 '_'
         top = map (\x -> padding ++ x ++ padding) half
         bot = map (\x -> x ++ "_" ++ x) half

loop 0 _ = putStrLn ""
loop n s = do
           threadDelay 500000
           putStr "\ESC[2J"
           putStrLn $ unlines s
           putStrLn $ show n
           loop (n-1) (iter s)

