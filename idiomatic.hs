import Control.Monad
import Control.Exception

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

        catch (print $ 5 `div` 0) handler
            where handler :: SomeException -> IO ()
                  handler = \_ -> putStrLn "."


