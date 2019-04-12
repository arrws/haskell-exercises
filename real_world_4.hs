import Control.Concurrent
import Control.Exception
import qualified Data.Map as M


instance Eq SomeException where
    (SomeException a) == (SomeException b) = True -- a == b

data ThreadStatus = Running
                  | Finished
                  | Threw SomeException
                  deriving (Eq, Show)


newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)


newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            result <- try body
            putMVar state (either Threw (const Finished) result)
        return (M.insert tid state m, tid)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
    modifyMVar mgr $ \m ->
    case M.lookup tid m of
        Nothing -> return (m, Nothing)
        Just st -> tryTakeMVar st >>= \mst -> case mst of
            Nothing -> return (m, Just Running)
            Just sth -> return (M.delete tid m, Just sth)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
    maybeDone <- modifyMVar mgr $ \m ->
        return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
            (Nothing, _) -> (m, Nothing)
            (done, m') -> (m', done)
    case maybeDone of
        Nothing -> return Nothing
        Just st -> Just `fmap` takeMVar st

waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)


fun = do
        let compute = (+) <$> [0..9] <*> [0..999]
        print $ show $ compute !! 5319

main = do
        m <- newManager
        id <- forkManaged m fun
        status <- getStatus m id
        case status of
          Nothing -> print "FAIL"
          Just st -> print "ok"
        return ()


