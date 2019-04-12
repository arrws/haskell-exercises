import Data.Array
import Control.Applicative
import Control.Monad
-- import Control.Monad.Trans

import System.Time
import qualified Data.ByteString.Lazy as L
import System.Console.Readline (readline)
import Control.Exception
import Control.Concurrent (forkIO)

-- --- Maybe
-- x >>= f = case x of
--             Nothing -> Nothing
--             Just value -> f value

-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)  }

-- (>>=) :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
-- x >>= f = MaybeT $
--             do unwrapped <- runMaybeT x
--                case unwrapped of
--                     Nothing    -> return Nothing
--                     Just value -> runMaybeT $ f value

-- returnM :: (Monad m) => a -> MaybeT m a
-- returnM = MaybeT . return . Just

-- failM :: (Monad m) => t -> MaybeT m a
-- failM _ = MaybeT $ return Nothing


-- instance MonadTrans MaybeT where
--     lift = MaybeT . (liftM Just)


-- newtype State s a =
--     State { runState :: (s -> (a,s))  }

-- instance Monad (State s) where
--     return a        = State $ \s -> (a,s)
--     (State x) >>= f = State $ \s ->
--                         let (v,s') = x s
--                         in runState (f v) s'

-- newtype StateT s m a =
--     StateT { runStateT :: (s -> m (a,s))  }

-- instance (Monad m) => Monad (StateT s m) where
--     return a         = StateT $ \s -> return (a,s)
--     (StateT x) >>= f = StateT $ \s -> do
--                         (v,s') <- x s
--                         runStateT (f v) s'


-- instance (MonadPlus m) => MonadPlus (StateT s m) where
--     mzero = StateT $ \_ -> mzero
--     (StateT x1) `mplus` (StateT x2) = StateT $ \s -> (x1 s) `mplus` (x2 s)



loop_this =  do
    mline <- readline "say"
    print $ mline
    case mline of
        Nothing -> return ()
        Just "" -> return ()
        Just smth -> do
            handle handler $ do
                content <- L.readFile smth
                forkIO (fun smth content)
                return ()
            print $ "done"
            -- loop_this
        where fun path = L.writeFile (path ++ ".hahahahha")
              handler :: SomeException -> IO ()
              handler = print --- \_-> return ()


main = do
        t1 <- getClockTime
        t2 <- getClockTime >>= (\(TOD sec _) -> return sec)
        t3 <- getClockTime >>= (\(TOD sec _) -> return (TOD (sec + 60*60*5) 0))
        t4 <- toCalendarTime t3
        print $ t1
        print $ t2
        print $ t3
        print $ t4
        print $ ctMonth t4
        print $ normalizeTimeDiff $ diffClockTimes t1 t3

        loop_this


