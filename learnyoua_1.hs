import Data.Char
import Data.List
import Control.Applicative


-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))

-- instance Functor ((->) r) where
--     fmap = (.)


-- -- definition
-- class (Functor f) => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--     pure = Just
--     Nothing <*> _ = Nothing
--     (Just f) <*> something = fmap f something

-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]

-- instance Applicative ZipList where
--     pure x = ZipList (repeat x)
--     ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)


-- -- infix fmap
-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x


-- -- definition
-- sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA [] = pure []
-- sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA = foldr (liftA2 (:)) (pure [])

-- class Monoid m where
--     mempty :: m
--     mappend :: m -> m -> m
--     mconcat :: [m] -> m
--     mconcat = foldr mappend mempty


main = do
        print $ 100 -: (*3)
        -- line <- fmap (intersperse '-' . reverse . map toUpper) getLine
        -- putStrLn line

        let applic = fmap (*) [1,2,3]
        print $ fmap (\x -> x 9) applic
        let applic' = fmap (*) (Just 2)
        print $ fmap (\x -> x 9) applic'
        -- print $ fmap (\x -> x (Just 9)) applic' -- can't do ! go applicative

        print $ Just (+3) <*> Just (5)
        print $ Just (++"hahah") <*> Nothing
        print $ Just (++"hahah") <*> Just ("HAHA")
        print $ pure (+) <*> Just (1) <*> Just (2)
        print $ Just (+) <*> Just (1) <*> Just (2)

        -- pure f <*> x === fmap f x (== f <$> x)
        print $ (++) <$> Just("feed") <*> Just("back")
        print $ [(*0),(+100),(^2)] <*> [1,2,3]
        print $ [(+),(*)] <*> [1,2] <*> [3,4]
        print $ filter (<6) $ (+) <$> [1,2] <*> [3,4]

        -- linee <- (++) <$> getLine <*> getLine
        -- putStrLn $ linee

        print $ (+) <$> (+3) <*> (*100) $ 5
        print $ liftA2 (:) (Just 3) (Just [4])
        print $ (:) <$> Just 3 <*> Just [4]

        print $ sequenceA [Just 3, Just 2, Just 1]
        print $ sequenceA [Just 3, Nothing, Just 1]
        print $ sequenceA [(+3),(+2),(+1)] 3

        print $ map (\f -> f 7) [(>4),(<10),even]
        print $ sequenceA [(>4),(<10),even] 7
        print $ sequenceA [[3,5],[0,1],[2,4]]



