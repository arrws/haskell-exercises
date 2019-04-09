import Data.Array
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

-- data Writer w a = Writer { runWriter :: (a, w)  }
-- Reader r a = Reader {  runReader :: r -> a  }
-- State s a = State { runState :: s -> (a, s)  }


half :: Int -> Writer String Int
half x = do
            tell ("I just halved " ++ (show x) ++ "!")
            return (x `div` 2)

hello :: Reader String String
hello = do
        name <- ask
        return ("hello, " ++ name ++ "!\n")

bye :: Reader String String
bye = do
        name <- ask
        return ("bye, " ++ name ++ "!\n")

convo :: Reader String String
convo = do
        c1 <- hello
        c2 <- bye
        return $ c1 ++ c2

main = do
        print $ half 8 >>= half >>= half
        print $ half <=< half <=< half $ 8 --- monad composition
        print $ runWriter $ half 8
        putStrLn $ runReader convo $ "Hasky"





