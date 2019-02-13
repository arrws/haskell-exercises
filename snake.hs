import System.IO
import System.Console.ANSI
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Data.Monoid ((<>))

data Direction = J|K|H|L
    deriving (Eq, Show)

data Command = Quit | Pause | Go Direction | Die
    deriving (Eq, Show)

data Environment = Environment { snake :: Snake
                               , score :: Int
                               , command :: Command
                               , food :: Position
                               , rand :: R.StdGen
                               , limit :: Int
                               , delete_cell :: Maybe Position
                               , create_cell :: Maybe Position
                               } deriving (Show)

type Position =  (Int, Int)

type Snake = [Position]


advance :: Environment -> Command -> Environment
advance env Quit        = env { command = Quit }
advance env Pause       = env { command = Pause }
advance env (Go dir)
    | p == (food env)   = env' { score = 1 + (score env')
                               , food = new_food
                               , rand = new_rand
                               , delete_cell = Nothing
                               }
    | elem p (snake env)= env' { command = Die }
    | otherwise         = env' { snake = init (snake env') }
    where
        p = nextPosition env dir
        (new_food, new_rand) = randomPosition env'
        env' = env { command = Go dir
                   , snake = p : (snake env)
                   , delete_cell = Just $ last (snake env)
                   , create_cell = Just $ p
                   }

nextPosition :: Environment -> Direction -> Position
nextPosition env dir
    | p /= (snake env)!!1 = p
    | otherwise           = forward (opposite dir) (head $ snake env) (limit env)
    where
        p = forward dir (head $ snake env) (limit env)
        opposite x = case x of
                        H -> L
                        L -> H
                        J -> K
                        K -> J

forward :: Direction -> Position -> Int -> Position
forward dir (x,y) lim = (mod x' lim, mod y' lim)
                        where (x', y') = case dir of
                                        J -> (x+1, y)
                                        K -> (x-1, y)
                                        L -> (x, y+1)
                                        H -> (x, y-1)

randomPosition :: Environment -> (Position, R.StdGen)
randomPosition env = head $ dropWhile invalid (generator (rand env))
            where
                invalid (x, _) = elem x (snake env)
                generator g = r:generator g'
                            where
                                r@(_, g') = ((x, y), g2)
                                (x, g1) = R.randomR (1, (limit env)) g
                                (y, g2) = R.randomR (1, (limit env)) g1

parseInput :: Char -> Maybe Command
parseInput c = case c of
                'q' -> Just Quit
                'p' -> Just Pause
                'k' -> Just $ Go K
                'j' -> Just $ Go J
                'l' -> Just $ Go L
                'h' -> Just $ Go H
                _   -> Nothing

getInput :: Producer Command IO()
getInput = forever $ do
            c <- lift getChar
            case parseInput c of
                Nothing      -> return()
                (Just x)     -> yield x

delay :: Int -> Pipe b b IO ()
delay t = forever $ do
            lift $ threadDelay (t * 100000)
            await >>= yield

updateGame :: (Environment) -> IO()
updateGame env = do
                let l = div (limit env) 2
                case command env of
                    Quit -> do
                            clearScreen
                            setCursorPosition l (l-5)
                            putStrLn $ "You quit!"
                    Die  -> do
                            clearScreen
                            setCursorPosition l (l-5)
                            putStrLn $ "You are dead!"
                            setCursorPosition (l+1) (l-5)
                            putStrLn $ "Score: " ++ show (score env)
                    Pause-> return()
                    _    -> do
                            mapM_ (draw ' ') (delete_cell env)
                            mapM_ (draw '@') (create_cell env)
                            draw 'X' (food env)
                setCursorPosition (limit env) 0

draw :: Char -> Position -> IO ()
draw c (x, y) = do
                setCursorPosition x (2*y)
                putChar c
                setCursorPosition x (2*y+1)
                putChar ' '

playGame game = P.scan advance game id >-> takeUntilAfter invalid
                where
                    invalid x = not $ elem (command x) [Quit, Die]
                    takeUntilAfter :: Monad m => (a -> Bool) -> Pipe a a m ()
                    takeUntilAfter cond = do
                        v <- await
                        yield v
                        if cond v then takeUntilAfter cond else return ()

initEnv = Environment { snake = [(5, x)| x <- [5..7]]
                      , score = 0
                      , command = Go H
                      , food = (6, 6)
                      , rand = R.mkStdGen 0
                      , limit = 20
                      , delete_cell = Nothing
                      , create_cell = Nothing
                      }

initGame env = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    let l = limit env
    mapM_ (draw '*') [(0, x) | x <- [0..l+1]]
    mapM_ (draw '*') [(l+1, x) | x <- [0..l+1]]
    mapM_ (draw '*') [(x, 0) | x <- [0..l+1]]
    mapM_ (draw '*') [(x, l+1) | x <- [0..l+1]]

main = do
        initGame initEnv
        let run p = async $ runEffect p >> performGC
            from  = fromInput
            to    = toOutput

        (m_writer, m_reader) <- spawn unbounded
        (d_writer, d_reader) <- spawn $ latest $ command initEnv

        inputTask   <- run $ getInput >-> to (m_writer <> d_writer)
        -- inputTask   <- run $ getInput >-> to d_writer
        delayedTask <- run $ from d_reader >-> delay 1 >-> to m_writer
        drawingTask <- run $ for
                                (from m_reader >-> playGame initEnv)
                                (lift . updateGame)

        waitAny [inputTask , drawingTask]

