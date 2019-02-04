import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Pair a =
    Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
    a <- arbitrary
    return $ Pair a a

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = genPair

instance Eq a => EqProp (Pair a) where
    (=-=) = eq


data Listy a =
    Nil | Cons a (Listy a)
    deriving (Eq, Show)

append :: Listy a -> Listy a -> Listy a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor Listy where
    fmap _ Nil = Nil
    fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative Listy where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

-- instance Monad Listy where
-- return = pure
-- (>>=) Nil _ = Nil
-- (>>=) (Cons a Nil) f = f a
-- (>>=) (Cons a rest) f = f a `append` (rest >>= f)

genListy :: Arbitrary a => Gen (Listy a)
genListy = do
    a <- arbitrary
    l <- genListy
    frequency [ (1, return Nil)
              , (2, return (Cons a l))
              ]

instance Arbitrary a => Arbitrary (Listy a) where
    arbitrary = genListy

instance Eq a => EqProp (Listy a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let l = xs
                in take' 100 l
              ys' = let l = ys
                in take' 100 l

take' :: Int -> Listy a -> Listy a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)


data Validation e a =
    Failure' e | Success' a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
    pure a = Success' a
    (<*>) (Failure' e) (Failure' e') = Failure' (e <> e')
    (<*>) (Failure' e) _ = Failure' e
    (<*>) _ (Failure' e) = Failure' e
    (<*>) (Success' f) (Success' a) = Success' (f a)

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
genValidation = do
    a <- arbitrary
    b <- arbitrary
    elements [ Failure' a, Success' b  ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = genValidation

instance (Eq a, Eq b) => EqProp (Validation a b) where
    (=-=) = eq

trigger :: Validation String (String, String, String)
trigger = undefined


main = do
    let x = ("a", "b", "c")
    quickBatch $ functor (Pair x x)
    quickBatch $ applicative (Pair x x)
    quickBatch $ applicative (Cons x Nil)
    quickBatch $ applicative trigger

