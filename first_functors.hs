import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
      (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
      (fmap g (fmap f x)) == (fmap (g . f) x)


data Sum a b =
    First b
    | Second a
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First b) = First (f b)
    fmap f (Second a) = Second a

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
    a <- arbitrary
    b <- arbitrary
    elements [ First a, Second b ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = sumGen

type SumCompose =
    Fun Int Int ->
    Fun Int Int ->
    Sum String Int ->
    Bool


data Pair a =
    Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

pairyGen :: Arbitrary a => Gen (Pair a)
pairyGen = do
    a <- arbitrary
    return (Pair a a)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = pairyGen

type PairCompose =
    Fun Bool String ->
    Fun String String ->
    Pair Bool ->
    Bool


data List a =
    Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a rest) = Cons (f a) (fmap f rest)

listyGen :: Arbitrary a => Gen (List a)
listyGen = do
    a <- arbitrary
    elements [ Nil, Cons a Nil ]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = listyGen

type ListCompose =
    Fun Int String ->
    Fun String String ->
    List Int ->
    Bool


main = do
    quickCheck (functorIdentity :: Sum Int Int -> Bool)
    quickCheck (functorCompose' :: SumCompose)
    quickCheck (functorIdentity :: Pair Bool -> Bool)
    quickCheck (functorCompose' :: PairCompose)
    quickCheck (functorIdentity :: List Int -> Bool)
    quickCheck (functorCompose' :: ListCompose)

