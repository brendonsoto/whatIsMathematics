module CH1 where


-- There's the Hex package, but I'd like to try converting numbers to hex myself

{-  addition table
    - Creates a 2d array representing the addition of 0-(n-1) for a base n system
    - Limited to base 16 for simplicity
    - TODO How to pretty print? I.e. how to print in a table/grid?
-}

isBaseValid :: Int -> Bool
isBaseValid n = n > 2 && n < 36

-- convertNumToSymbol => returns an alphanumeric character to represent the max value of a base
-- To make things more convenient we're ignoring negative numbers
convertNumToSymbol :: Int -> Char
-- I need to do a better job at explaining things. i.e. why n-2?
convertNumToSymbol n =
  if n < 10
  then ['0'..'9'] !! n
  else ['a'..'z'] !! (n - 10)

type Base = Int
type Exponent = Int

getGreatestBaseExponent :: Base -> Int -> Int -> Int
getGreatestBaseExponent b a n =
  if b^a > n
  then a - 1
  else getGreatestBaseExponent b (a + 1) n

convertNum :: Base -> Int -> String
convertNum b n =
  if isBaseValid b
  then convertNum' b e n
  else error "Invalid number"
  where e = getGreatestBaseExponent b 0 n

convertNum' :: Base -> Exponent -> Int -> String
convertNum' b e n =
  if e == 0
  then convertNumToSymbol n : []
  else (convertNumToSymbol . fst $ dm) : (convertNum' b (e - 1) (snd dm))
  where dm = divMod n (b^e)
