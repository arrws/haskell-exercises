import Control.Monad

-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--     fail :: String -> m a
--     fail msg = error msg

-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= f = Nothing
--     Just x >>= f  = f x
--     fail _ = Nothing

-- instance Monad [] where
--     return x = [x]
--     xs >>= f = concat (map f xs)
--     fail _ = []


type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing


foo :: Maybe String
foo =   Just 3   >>= (\x ->
        Just "!" >>= (\y ->
        Just (show x ++ y)
        )
    )

foo' :: Maybe String
foo' = do
        x <- Just 3
        y <- Just "!"
        Just (show x ++ y)


-- class Monad m => MonadPlus m where
--     mzero :: m a
--     mplus :: m a -> m a -> m a

-- instance MonadPlus [] where
--     mzero = []
--     mplus = (++)

-- guard :: (MonadPlus m) => Bool -> m ()
-- guard True = return ()
-- guard False = mzero


type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
            (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                       ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                       ]
            guard (c' `elem` [1..8] && r' `elem` [1..8])
            return (c',r')


-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = (\x -> f (g x))

-- (<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
-- f <=< g = (\x -> g x >>= f)


main = do
        print $ return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
        print $ return (0,0) >>= landLeft 1 >>= landRight 2 >>= landLeft (1) >>= landRight (-1)
        print $ return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
        print $ foo
        print $ foo'
        print $ [3,4,5] >>= \x -> [x,-x]
        print $ [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
        print $ [ (n,ch) | n<-[1,2], ch<-['a','b'] ]
        print $ [1..10] >>= (\x -> guard (odd x) >> return x)
        print $ return (0,0) >>= moveKnight >>= moveKnight>>= moveKnight
        let f x = [x,-x]
        let g x = [x*3,x*2]
        let h = f <=< g
        print $ h 3


