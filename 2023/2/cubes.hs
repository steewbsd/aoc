import Data.Either
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

-- Possible Cube colors
data CubeColor = Red | Blue | Green deriving (Show, Eq)

-- Cube type, with number of cubes and the color
data Cube = Cube Int CubeColor deriving (Show)

colorFromStr :: String -> Maybe CubeColor
colorFromStr "red" = Just Red
colorFromStr "blue" = Just Blue
colorFromStr "green" = Just Green
colorFromStr _ = Nothing

number :: GenParser Char st Int
number = do
  nn <- many1 digit
  return (read nn :: Int)

color :: GenParser Char st CubeColor
color = do
  c <- string "red" <|> string "blue" <|> string "green"
  return $ fromJust $ colorFromStr c

gameNumParser :: GenParser Char st Int
gameNumParser =
  do
    string "Game"
    spaces
    x <- number
    char ':'
    spaces
    return x

cubeParser :: GenParser Char st Cube
cubeParser = do
  spaces
  n <- number
  spaces
  Cube n <$> color

lineParser :: GenParser Char st Game
lineParser =
  do
    spaces
    game <- gameNumParser
    cubes <-
      many1
        ( do
            x <- cubeParser
            char ',' <|> char ';'
            return x
        )
    eof
    return $ Game game cubes

incrementCube :: Cube -> (Cube, Cube, Cube) -> (Cube, Cube, Cube)
incrementCube (Cube n Red) (Cube a _, g, b) = (Cube (n + a) Red, g, b)
incrementCube (Cube n Green) (r, Cube a _, b) = (r, Cube (n + a) Green, b)
incrementCube (Cube n Blue) (r, g, Cube a _) = (r, g, Cube (n + a) Blue)

--                     Red   Green Blue
sumCubes :: [Cube] -> [CubeColor] -> [Cube]
sumCubes _ [] = []
sumCubes c matchAgainst = Cube n match : sumCubes c restToMatch -- (sum . fst $ split) (head c):sumCubes
  where
    match = head matchAgainst
    split = filter (\(Cube _ m) -> m == match) c
    n = foldr (\(Cube n _) acc -> n + acc) 0 split
    restToMatch = tail matchAgainst

maxRed = 12 :: Int

maxGreen = 13 :: Int

maxBlue = 14 :: Int

data Game = Game
  { num :: Int,
    cubes :: [Cube]
  }
  deriving (Show)

main :: IO ()
main = do
  f <- readFile "input"
  print $ foldr (\ (Game n _) acc -> acc + n) 0
    -- $ filter (\(Game num (Cube nred Red : Cube ngreen Green : Cube nblue Blue : _)) -> nred < maxRed && nblue < maxBlue && ngreen < maxGreen)
    $ filter (\(Game num cubes) -> not $ any (\(Cube n c) -> case c of
                                                          Red -> n > maxRed
                                                          Green -> n > maxGreen
                                                          Blue -> n > maxBlue) cubes)
    $ map
      ( \l -> case parse lineParser "Error" l of
          Left _ -> Game 0 []
          Right game -> game -- Game (num game) (sumCubes (cubes game) [Red, Green, Blue])
      )
    $ lines f
