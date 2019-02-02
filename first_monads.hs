import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Nope a =
    NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq


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

main = do
    testWithTrigger (undefined :: Nope (Int, Int, Int))
    testWithTrigger (undefined :: MyEither String (Int, Int, Int))
    testWithTrigger (undefined :: Identity (Int, Int, Int))

