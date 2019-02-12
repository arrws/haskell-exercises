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

data State = Quited | Paused | Running | Lost
    deriving (Eq, Show)

data Command = Quit | Pause | Go Direction
    deriving (Eq, Show)

data Environment = Environment { snake :: Snake
                               , score :: Int
                               , direction :: Command
                               , food :: Position
                               , rand :: R.StdGen
                               , limit :: Int
                               , state :: State
                               } deriving (Show)


type Position =  (Int, Int)

type Snake = [Position]

opposite :: Direction -> Direction
opposite x = case x of
                H -> L
                L -> H
                J -> K
                K -> J

nextPosition :: Direction -> Position -> Int -> Position
nextPosition dir (x,y) lim = (mod new_x lim, mod new_y lim)
                            where (new_x, new_y) = case dir of
                                                J -> (x+1, y)
                                                K -> (x-1, y)
                                                L -> (x, y+1)
                                                H -> (x, y-1)

randomPosition :: Environment -> (Position, R.StdGen)
randomPosition env = head $ dropWhile invalid (positions (rand env))
            where
                invalid (x, _) = x `elem` (snake env)
                positions g = r:positions g'
                    where r@(_, g') = tryrand g
                tryrand g =
                        let (x, g1) = R.randomR (1, (limit env)) g
                            (y, g2) = R.randomR (1, (limit env)) g1
                            in ((x, y), g2)

advance :: Environment -> Command -> Environment
advance env Quit        = env { state = Quited }
advance env Pause       = env { state = Paused }
advance env (Go dir)    = do
                        let p = nextPosition dir (head $ snake env) (limit env)
                        if p /= (snake env) !! 1
                        then check $ p
                        else check $ nextPosition (opposite dir) (head $ snake env) (limit env)
                        where
                            check p
                                | p == (food env)     = let (new_food, new_rand) = randomPosition env in
                                                        env { direction = Go dir
                                                            , snake = p : (snake env)
                                                            , score = 1 + (score env)
                                                            , food = new_food
                                                            , rand = new_rand
                                                            , state = Running
                                                            }
                                | elem p (snake env)  = env { direction = Go dir
                                                            , state = Lost
                                                            }
                                | otherwise           = env { direction = Go dir
                                                            , snake = p : init (snake env)
                                                            , state = Running
                                                            }


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
getInput = getit >-> loopit
        where
            getit = forever $ do
                    c <- lift getChar
                    case parseInput c of
                        Nothing      -> return()
                        (Just x)     -> yield x

            loopit = do
                        first <- await
                        yield first
                        loop first
                        where
                            loop prev = do
                                next <- await
                                yield next
                                loop next

                -- check (Go x) (Go y) = x == opposite y
                -- check _ _ = False
                -- loop prev = do
                    -- next <- await
                    -- if check next prev
                    -- then loop prev
                    -- else yield next >> loop next


delay :: Int -> Pipe b b IO ()
delay t = forever $ do
        lift $ threadDelay (t * 100000)
        await >>= yield

updateGame :: (Environment, Environment) -> IO()
updateGame (prev_env, env)
    | state env == Quited = do
                            let l = div (limit env) 2
                            clearScreen
                            setCursorPosition l (l-5)
                            putStrLn $ "You quit!"
                            setCursorPosition (limit env) 0

    | state env == Paused = return()

    | state env == Running  = do
                            mapM_ (draw ' ') (reverse $ snake prev_env)
                            mapM_ (draw '@') (reverse $ snake env)
                            draw 'X' (food env)
                            setCursorPosition (limit env) 0

    | state env == Lost     = do
                            let l = div (limit env) 2
                            clearScreen
                            setCursorPosition l (l-5)
                            putStrLn $ "You died!"
                            setCursorPosition (l+1) (l-5)
                            putStrLn $ "Score: " ++ show (score env)
                            setCursorPosition (limit env) 0

draw :: Char -> Position -> IO ()
draw c (x, y) = setCursorPosition x y >> putChar c

playGame game =
                P.scan advance game id
                >-> keepPrev -- give last 2 Evironments
                >-> P.takeWhile (\(x,y) -> elem (state x) [Running, Paused])
                where
                    keepPrev = do
                        first <- await
                        P.scan remember (first, first) id
                        where
                        remember (_, a) b = (a, b)

initEnv = Environment { snake = [(5, x)| x <- [2..17]]
                      , score = 0
                      , direction = Go H
                      , food = (6, 6)
                      , rand = R.mkStdGen 0
                      , limit = 20
                      , state = Running
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

        let
            run p = async $ runEffect p >> performGC
            from  = fromInput
            to    = toOutput

        (mO, mI) <- spawn unbounded
        (dO, dI) <- spawn $ latest $ direction initEnv

        inputTask   <- run $ getInput >-> to (mO <> dO)
        delayedTask <- run $ from dI >-> delay 3 >-> to mO
        drawingTask <- run $ for
                                (from mI >-> playGame initEnv)   -- producer
                                (lift . updateGame)              -- effect

        waitAny [inputTask , drawingTask]

