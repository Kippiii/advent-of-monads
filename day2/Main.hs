import Text.ParserCombinators.ReadP
import Data.Char

data Round = Round {numRed :: Int, numGreen :: Int, numBlue :: Int}
data Game = Game {getId :: Int, getRounds :: [Round]}

tupMax :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupMax (x1, y1, z1) (x2, y2, z2) = (max x1 x2, max y1 y2, max z1 z2)

toTup :: Round -> (Int, Int, Int)
toTup r = (numRed r, numGreen r, numBlue r)

getPower :: Game -> Int
getPower game =
    let (r, g, b) = foldl tupMax (0, 0, 0) $ map toTup $ getRounds game
    in r*g*b

getAllPower :: [Game] -> Int
getAllPower = sum . map getPower

parseInput :: ReadP [Game]
parseInput = do
    skipSpaces
    games <- parseGames
    eof
    return games

parseGames :: ReadP [Game]
parseGames = many parseGame

parseGame :: ReadP Game
parseGame = do
    parseWord "Game"
    id <- parseInt
    parseStr ":"
    rounds <- parseRounds
    return $ Game id rounds

parseRounds :: ReadP [Round]
parseRounds = sepBy1 parseRound (parseStr ";")

parseRound :: ReadP Round
parseRound = do
    colorAgg <- sepBy1 parseColor (parseStr ",")
    let (r, g, b) = foldl (\(x1, y1, z1) (x2, y2, z2) -> (x1+x2, y1+y2, z1+z2)) (0, 0, 0) colorAgg
    return $ Round r g b

parseColor :: ReadP (Int, Int, Int)
parseColor = do
    num <- parseInt
    (
        (do
            parseWord "red"
            return (num, 0, 0)
        )
        +++
        (do
            parseWord "green"
            return (0, num, 0)
        )
        +++
        (do
            parseWord "blue"
            return (0, 0, num)
        ))

parseWord :: String -> ReadP ()
parseWord exp = do
    obt <- munch1 isLetter
    skipSpaces
    if obt == exp then
        return ()
    else
        pfail

parseStr :: String -> ReadP ()
parseStr s = do
    string s
    skipSpaces

parseInt :: ReadP Int
parseInt = do
    istr <- munch1 isDigit
    skipSpaces
    return $ read istr

getAns :: String -> Int
getAns input =
    case readP_to_S parseInput input of
        [] -> error "Could not parse input!"
        [(games, "")] -> getAllPower games
        _ -> error "Grammar is ambiguous!"

main :: IO ()
main = interact (show . getAns)