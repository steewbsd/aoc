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

lineParser :: GenParser Char st [Cube]
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
    return cubes

incrementCube :: Cube -> (Cube, Cube, Cube) -> (Cube, Cube, Cube)
incrementCube (Cube n Red) (r, g, b) = (Cube (n+1) Red, g, b)
incrementCube (Cube n Green) (r, g, b) = (r, Cube (n+1) Green, b)
incrementCube (Cube n Blue) (r, g, b) = (r, g, Cube (n+1) Blue)

--                     Red   Green Blue
sumCubes :: [Cube] -> (Cube, Cube, Cube)
sumCubes = foldr (\c acc -> incrementCube c acc) (Cube 0 Red, Cube 0 Green, Cube 0 Blue) 

main :: IO ()
main = do
  f <- readFile "reduced"
  print
    $ map
      ( \l -> case parse lineParser "Error" l of
          Left _ -> (Cube 0 Red, Cube 0 Green, Cube 0 Blue)
          Right c -> trace (show c) sumCubes c
      )
    $ lines f
