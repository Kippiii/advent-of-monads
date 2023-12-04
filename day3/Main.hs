import Data.Char (isDigit, isSpace)
import Control.Monad (liftM, ap, filterM)
import Control.Monad.Trans (MonadTrans, lift)

data Location = Location Int Int deriving Show
data GridNum = GridNum {getVal :: Int, getStart :: Location, getEnd :: Location} deriving Show
data State = State [Location] [GridNum]

newtype Env a = Env {runEnv :: State -> (a, State)}

instance Functor Env where
    fmap = liftM
instance Applicative Env where
    pure a = Env $ \s -> (a, s)
    (<*>) = ap
instance Monad Env where
    return = pure
    m >>= f = Env $ \s1 ->
        let (a, s2) = runEnv m s1
        in  runEnv (f a) s2

insertLocation :: Location -> Env ()
insertLocation l = Env $ \(State locs nums) -> ((), State (l:locs) nums)

insertGridNum :: GridNum -> Env ()
insertGridNum n = Env $ \(State locs nums) -> ((), State locs (n:nums))

getLocations :: Env [Location]
getLocations = Env $ \(State locs nums) -> (locs, State locs nums)

getGridNums :: Env [GridNum]
getGridNums = Env $ \(State locs nums) -> (nums, State locs nums)

unpackEnv :: Env a -> a
unpackEnv env = fst $ runEnv env (State [] [])

newtype BuilderT m a = BuilderT {runBuilder :: (Int, Int) -> m (a, (Int, Int))}

instance (Monad m) => Functor (BuilderT m) where
    fmap = liftM
instance (Monad m) => Applicative (BuilderT m) where
    pure a = BuilderT $ \c -> pure (a, c)
    (<*>) = ap
instance (Monad m) => Monad (BuilderT m) where
    return = pure
    m >>= f = BuilderT $ \c1 -> do
        (a, c2) <- runBuilder m c1
        runBuilder (f a) c2
instance MonadTrans BuilderT where
    lift m = BuilderT $ \c -> m >>= (\a -> return (a, c))

moveRight :: (Monad m) => BuilderT m ()
moveRight = BuilderT $ \(x, y) -> return ((), (x+1, y))

moveDown :: (Monad m) => BuilderT m ()
moveDown = BuilderT $ \(_, y) -> return ((), (0, y+1))

getCoords :: (Monad m) => BuilderT m (Int, Int)
getCoords = BuilderT $ \c -> return (c, c)

unpackBuilder :: (Functor m) => BuilderT m a -> m a
unpackBuilder b = fmap fst $ runBuilder b (0, 0)

intSplit :: String -> (String, String)
intSplit "" = ("", "")
intSplit (c:rest)
    | isDigit c =
            let (iStr, garbage) = intSplit rest in (c:iStr, garbage)
    | otherwise = ("", c:rest)

parseLine :: String -> BuilderT Env ()
parseLine "" = return ()
parseLine ('.':rest) = do
    moveRight
    parseLine rest
parseLine ('*':rest) = do
    (x, y) <- getCoords
    lift $ insertLocation $ Location x y
    moveRight
    parseLine rest
parseLine s =
    case intSplit s of
        ("", _:rest) -> do
            moveRight
            parseLine rest
        (iStr, rest) -> do
            (x, y) <- getCoords
            lift $ insertGridNum $ GridNum (read iStr) (Location x y) (Location (x + length iStr - 1) y)
            sequence $ fmap (\_ -> moveRight) iStr
            parseLine rest

parseLines :: [String] -> BuilderT Env ()
parseLines [] = return ()
parseLines (s:rest) = do
    parseLine s
    moveDown
    parseLines rest

isByNum :: Location -> GridNum -> Bool
isByNum (Location symbX symbY) (GridNum _ (Location startX startY) (Location endX endY)) =
    symbX <= endX + 1 && symbX >= startX - 1 && symbY <= endY + 1 && symbY >= startY - 1

getGearRatio :: Location -> Env Int
getGearRatio loc = do
    nums <- getGridNums
    let near = filter (isByNum loc) nums
    if length near == 2 then
        return $ product $ map getVal near
    else
        return 0

scoreCalculator :: Env Int
scoreCalculator = do
    gears <- getLocations
    fmap sum $ sequence $ map getGearRatio gears

calcScore :: [String] -> Env Int
calcScore ss = do
    unpackBuilder $ parseLines ss
    scoreCalculator

getAns :: String -> String
getAns input = show $ unpackEnv $ calcScore $ lines input

main :: IO ()
main = interact getAns