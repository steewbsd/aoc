import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import System.IO

literalDigit :: String -> Maybe Int
literalDigit "one" = Just 1
literalDigit "two" = Just 2
literalDigit "three" = Just 3
literalDigit "four" = Just 4
literalDigit "five" = Just 5
literalDigit "six" = Just 6
literalDigit "seven" = Just 7
literalDigit "eight" = Just 8
literalDigit "nine" = Just 9
literalDigit "zero" = Just 0
literalDigit _ = Nothing

literals :: [[Char]]
literals = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

isValid :: [Char] -> [String] -> [Char] -> [String]
isValid [] valid _ = valid
isValid full@(s : xs) valid strcat -- actual string, valid elems list, part of the concat string if any
  | isDigit s = isValid xs (valid ++ [[s]]) [] -- append digit to valid list
  | literal = isValid xs (valid ++ [cat]) []
  | or isPrefix = isValid xs valid cat
  | or isNextPrefix = isValid xs valid [s]
  | or isPrevPrefix = isValid xs valid (previous ++ [s])
  | otherwise = isValid xs valid []
  where
    cat = strcat ++ [s]
    previous = [last strcat | length strcat > 1]
    isPrefix = map (isPrefixOf cat) literals
    isNextPrefix = map (isPrefixOf [s]) literals
    isPrevPrefix = map (isPrefixOf $ previous ++ [s]) literals
    literal = cat `elem` literals

parseLiteralOrDigit :: String -> Int
parseLiteralOrDigit [] = error "Empty string passed..."
parseLiteralOrDigit full@(l : ls)
  | isDigit l = digitToInt l
  | isJust literal = fromJust literal
  | otherwise = trace full error "Couldn't parse int..."
  where
    literal = literalDigit full

getDigits :: String -> Int
getDigits s =
  if length digs > 1
    then
      (10 * parseLiteralOrDigit (head digs))
        + parseLiteralOrDigit (last digs)
    else parseLiteralOrDigit $ head digs
  where
    digs = isValid s [] []

main :: IO ()
main = do
  contents <- readFile "input"
  print $ map getDigits $ lines contents
