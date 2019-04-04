-- identitiy id
idiot :: a -> a
idiot = id

-- self application
mock :: (a -> a) -> a -> a
mock f x = f (f x)

-- true first const
kestrel :: a -> a -> a
kestrel a b = a
tr = kestrel

-- false second
kite :: a -> a -> a
kite a b = b
fl = kite

-- not flip
cardinal :: (a -> a -> a) -> a -> a -> a
cardinal f a b = f b a

-- composition (.)
bluebird :: (b -> c) -> (a -> b) -> a -> c
bluebird = (.)

-- hold an arg
-- flip id
-- thrush :: a -> (a -> b) -> b

-- hold a pair of args
-- flip . flip id
-- vireo :: a -> b -> (a -> b -> c) -> c


-- successor of n
suc :: ((a -> b) -> c -> a) -> (a -> b) -> c -> b
suc n f x = f (n f x)

unbool :: (Bool -> Bool -> t) -> t
unbool n = n True False

unchurch :: ((Int -> Int) -> Int -> a) -> a
unchurch n = n (\i -> i + 1) 0

church :: Int -> (a -> a) -> a ->a
church n =
    if n == 0
    then zero
    else \f x -> f (church (n-1) f x)

zero :: a -> b -> b
zero a b = b

one :: (a -> a) -> a -> a
one = suc zero


test :: (Num a) => a -> a
test a = a+2


main = do
    print (unchurch ( suc one ) )
    print (unchurch ( suc ( suc ( suc one ) ) ) )
    print (unchurch (church 41))
    print( mock test 8 )
    print( kestrel 1 2 )
    print( kite 1 2 )
    print( cardinal kestrel 1 2 )

