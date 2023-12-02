import Data.Either
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

-- Possible Cube colors
data CubeColor = Red | Blue | Green deriving (Show)

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
  c <- color
  return $ Cube n c

lineParser :: GenParser Char st (Int, [Cube])
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
    return (game, cubes)

incrementCube :: Cube -> (Cube, Cube, Cube) -> (Cube, Cube, Cube)
incrementCube (Cube n Red) (Cube a _, g, b) = (Cube (n + a) Red, g, b)
incrementCube (Cube n Green) (r, Cube a _, b) = (r, Cube (n + a) Green, b)
incrementCube (Cube n Blue) (r, g, Cube a _) = (r, g, Cube (n + a) Blue)

--                     Red   Green Blue
sumCubes :: [Cube] -> (Cube, Cube, Cube)
sumCubes = foldr incrementCube (Cube 0 Red, Cube 0 Green, Cube 0 Blue)

maxRed = 12 :: Int

maxGreen = 13 :: Int

maxBlue = 14 :: Int

main :: IO ()
main = do
  f <- readFile "input"
  print
    $ filter
    $ (\g c -> fst c < maxRed && snd c < maxGreen)
      . map
        ( \l -> case parse lineParser "Error" l of
            Left _ -> (0, (Cube 0 Red, Cube 0 Green, Cube 0 Blue))
            Right (g, c) -> (g, sumCubes c)
        )
    $ lines f
