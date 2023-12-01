import Data.Char (isDigit)
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

-- Stores an updating integer counter (Reader)
newtype Treb a = Treb {runTreb :: (Int, a)}

instance Functor Treb where
    fmap = liftM
instance Applicative Treb where
    pure a = Treb (0, a)
    (<*>) = ap
instance Monad Treb where
    return = pure
    m >>= f =
        let (n1, a1) = runTreb m
            (n2, a2) = runTreb $ f a1
        in Treb (n1 + n2, a2)

-- Gets the integer associated with a Treb
getResult :: (Treb a) -> Int
getResult t =
    let (n, _) = runTreb t
    in n

-- Helper util (could not import)
startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (c1:rest1) (c2:rest2)
    | c1 == c2 = startsWith rest1 rest2
    | otherwise = False

-- Checks if a string starts with any string from a list. Returns index
checkAgainst :: String -> [String] -> Maybe Int
checkAgainst s [] = Nothing
checkAgainst s (cur:rest) =
    if startsWith s cur then
        Just 0
    else
        case checkAgainst s rest of
            Just i -> Just $ i+1
            Nothing -> Nothing

-- Checks for a number in a string
check :: [String] -> String -> String
check _ "" = error "No number in string"
check arr s@(c:rest)
    | isDigit c = [c]
    | otherwise =
        case checkAgainst s arr of
            Just i -> show $ i+1
            Nothing -> check arr rest

-- The list of all numbers as strings
numbers :: [String]
numbers = [
      "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"]

getFirst :: String -> String
getFirst = check numbers

getSecond :: String -> String
getSecond = check (map reverse numbers) . reverse

-- Converts a string into its numeric form
makeTreb :: String -> Treb ()
makeTreb s =
    let n = read $ getFirst s ++ getSecond s
    in Treb (n, ())

getAns :: [String] -> Int
getAns arr =
    let helper [] = return ()
        helper (v:rest) = do
            makeTreb v
            helper rest
    in getResult $ helper arr

main :: IO ()
main = interact (show . getAns . lines)
