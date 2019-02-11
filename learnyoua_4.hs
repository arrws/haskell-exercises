import Control.Monad
import Control.Monad.Instances
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State (StateT)

-- newtype State s a = State { runState :: s -> (a,s)  }
type Stack = [Int]
type State s = StateT s Identity


instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState

pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
            push 3
            pop
            pop

stackStuff :: State Stack ()
stackStuff = do
            a <- pop
            if a == 5
            then push 5
            else do
                push 3
                push 8

moreStack :: State Stack ()
moreStack = do
            a <- stackManip
            if a == 100
            then stackStuff
            else return ()

get = State $ \s -> (s,s)
put newState = State $ \s -> ((),newState)


newtype Writer w a = Writer { runWriter :: (a, w)  }
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
        | b == 0 = do
                    tell ["Finished with " ++ show a]
                    return a
        | otherwise = do
                    tell [show a ++ " mod  " ++ show b ++ " =  " ++ show (a `mod` b)]
                    gcd' b (a `mod` b)
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
            | b == 0 = do
                        tell ["Finished with " ++ show a]
                        return a
            | otherwise = do
                        result <- gcdReverse b (a `mod` b)
                        tell [show a ++ " mod  " ++ show b ++ " =  " ++ show (a `mod` b)]
                        return result


newtype DiffList a = DiffList { getDiffList :: [a] -> [a]  }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcdDiffList :: Int -> Int -> Writer (DiffList String) Int
gcdDiffList a b
        | b == 0 = do
                    tell (toDiffList ["Finished with " ++ show a])
                    return a
        | otherwise = do
                    result <- gcdDiffList b (a `mod` b)
                    tell (toDiffList [show a ++ " mod  " ++ show b ++ " =  " ++ show (a `mod` b)])
                    return result


addStuff :: Int -> Int
addStuff x = let
                a = (*2) x
                b = (+10) x
                in a+b
-- addStuff = do
--             a <- (*2)
--             b <- (+10)
--             return (a+b)


main = do
    print $ runWriter (gcd' 8 3)
    mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
    mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
    mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdDiffList 110 34
    print $ addStuff 3
    print $ runState stackManip [5,8,2,1]
    print $ runState stackStuff [9,0,2,1,0]

