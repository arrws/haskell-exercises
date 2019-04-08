import Control.Monad
import Control.Exception
import Data.List
import Test.QuickCheck
import Control.Applicative
import System.Random
import Control.Monad.Trans.State


-- map :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- (<$>) :: Functor t     =>   (a ->   b) -> (t a -> t b)
-- (<*>) :: Applicative t => t (a ->   b) -> (t a -> t b)
-- (=<<) :: Monad t       =>   (a -> t b) -> (t a -> t b)


pythags = do
            z <- [1..]
            x <- [1..z]
            y <- [x..z]
            guard (x^2 + y^2 == z^2)
            return (x, y, z)


handler :: SomeException -> IO (Maybe a)
handler = \_ -> return Nothing

saferFileSize path = handle handler $ do
                    -- h <- something
                    return (Just 3)


qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort gr
            where
                ls = filter (<x) xs
                gr = filter (>=x) xs


prop_check x = not (null x) ==> head (qsort x) == minimum x



roll_dice :: IO (Int, Int)
roll_dice = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))

roll_dice' :: IO (Int, Int)
roll_dice' = (,) <$> (randomRIO (1,6)) <*> (randomRIO (1,6))

roll_dice'' :: State StdGen Int
roll_dice'' = state $ randomR (1,6)
-- roll_dice'' = do
--                 g <- get
--                 let (v, g') = randomR (1,6) g
--                 put g'
--                 return v

get_random :: Random a => State StdGen a
get_random = state random

get_rand_types :: State StdGen (Int, Double, Char, Bool)
get_rand_types = (,,,) <$> get_random
                        <*> get_random
                        <*> get_random
                        <*> get_random


main = do
        print $ filterM (const [True, False]) [1, 2, 3]
        print $ map ($3) [odd, even]
        print $ sequence [odd, even] 3
        print $ liftM2 (&&) odd even 3
        print $ liftM2 (+) (Just 3) (Just 1)

        let l1 = (++) <$> getLine <*> getLine
            l2 = (++) <$> getLine <*> (take 2 <$> getLine)
        (\_ y -> y) <$> putStrLn "First!" <*> putStrLn "Second!"
        putStrLn "First!" *> putStrLn "Second!"

        print $ take 5 pythags

        print $ [0,0,0] >>= \x -> replicate x x
        print $ [0,0,0] >>= \x -> replicate x x
        return 3 >>= \x -> print $ if x < 10 then "Too small" else "OK"
        return 42 >>= \x -> print $ if x < 10 then "Too small" else "OK"

        -- catch (print $ 5 `div` 0) handler
        --     where handler :: SomeException -> IO ()
        --           handler = \_ -> putStrLn "."

        -- quickCheck (prop_check :: [Int] -> Property)

        roll_dice
        roll_dice'

        let seed = (mkStdGen 0)

        print $ runState roll_dice'' seed
        print $ evalState roll_dice'' seed
        print $ execState roll_dice'' seed

        -- print $ evalState get_random seed :: Bool
        -- print $ evalState get_random seed :: Double
        -- print $ evalState get_random seed :: Char
        -- print $ evalState get_random seed :: Char

        -- evalState get_rand_types seed

