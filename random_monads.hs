import System.Random
import Control.Monad

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
    let g = mkStdGen 666
    print $ test 0 g
    print $ seqm [Just 3, Just 4]
    print $ seqm [Just 3, Just 4, Nothing, Just 8]
    print $ seqm [[1,2,3],[10,20,30]]

