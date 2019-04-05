import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import System.Random


newtype Identity a =
    Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq


data MyEither a b =
    Left' a | Right' b
    deriving (Eq, Show)

instance Functor (MyEither a) where
    fmap f (Left' a) = Left' a
    fmap f (Right' b) = Right' (f b)

instance Applicative (MyEither a) where
    pure = Right'
    (<*>) (Left' a) _ = Left' a
    (<*>) _ (Left' a) = Left' a
    (<*>) (Right' f) (Right' x) = Right' (f x)

instance Monad (MyEither a) where
    return = pure
    (>>=) (Left' a) _  = Left' a
    (>>=) (Right' b) f = f b

genEither :: (Arbitrary a, Arbitrary b) => Gen (MyEither a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyEither a b) where
    arbitrary = genEither

instance (Eq a, Eq b) => EqProp (MyEither a b) where
    (=-=) = eq


testWithTrigger trigger = do
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger


bind :: (a -> StdGen -> (b, StdGen)) -> (StdGen -> (a, StdGen)) -> (StdGen -> (b, StdGen))
bind f x g = let (x',g') = x g in f x' g'

addDigit :: Integer -> StdGen -> (Integer, StdGen)
addDigit n g = let (a, g') = random g in (n + a `mod` 10, g')

unit :: Integer -> StdGen -> (Integer, StdGen)
unit x g = (x,g)

shift :: Integer -> (StdGen -> (Integer, StdGen))
shift = unit . (*5)

test :: Integer -> StdGen ->  (Integer, StdGen)
test = bind addDigit . bind shift . addDigit



seqm :: Monad m => [m a] -> m [a]
seqm = foldr mcons (return [])

mcons :: Monad m => m t -> m [t] -> m [t]
mcons p q = do
    x <- p
    y <- q
    return (x:y)


main = do
    testWithTrigger (undefined :: MyEither String (Int, Int, Int))
    testWithTrigger (undefined :: Identity (Int, Int, Int))
    let g = mkStdGen 666
    print $ test 0 g
    print $ seqm [Just 3, Just 4]
    print $ seqm [Just 3, Just 4, Nothing, Just 8]
    print $ seqm [[1,2,3],[10,20,30]]


