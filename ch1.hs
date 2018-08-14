module CH1 where


-- There's the Hex package, but I'd like to try converting numbers to hex myself
import Data.Char

{-  addition table
    - Creates a 2d array representing the addition of 0-(n-1) for a base n system
    - Limited to base 16 for simplicity
    - TODO How to pretty print? I.e. how to print in a table/grid?
-}


-- TODO Find a way of adding base 0 in by default instead of having to call `getHexRepresentation 321 0`
getHexRepresentation :: Int -> Int -> String
getHexRepresentation x y
  | y < 0 = ""
  | x >= 16 ^ (y + 1) = getHexRepresentation x (y + 1)
  | otherwise = intToDigit (x `div` (16 ^ y)) : getHexRepresentation (x `mod` (16 ^ y)) (y - 1)


getAdditionTable :: Int -> [String]
getAdditionTable n = map (flip getHexRepresentation 0) [x + y | x <- [0..n], y <- [0..n]]

getMultiplicationTable :: Int -> [String]
getMultiplicationTable n = map (flip getHexRepresentation 0) [x * y | x <- [0..n], y <- [0..n]]


prettyPrint :: Int -> [String] -> [[String]]
prettyPrint _ [] = []
prettyPrint n xs = take n xs : prettyPrint n (drop n xs)
