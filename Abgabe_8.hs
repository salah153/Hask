isMatrix :: [[Int]] -> Bool
isMatrix [] = False
isMatrix [[]] = False
isMatrix [x0] = True
isMatrix (x0 : x1 : xs) = length x0 == length x1 && isMatrix (x1 : xs)

spalten :: [[Int]] -> Int
spalten (x : xs) = length x

zeilen :: [[Int]] -> Int
zeilen x = length x

dimensions :: [[Int]] -> (Int, Int)
dimensions x
  | not (isMatrix x) = (-1, -1)
  | otherwise = (zeilen x, spalten x)

isQuadratic :: [[Int]] -> Bool
isQuadratic x = spalten x == zeilen x

getRow :: [[Int]] -> Int -> [Int]
getRow x i
  | isMatrix x && i -1 > length x = []
  | isMatrix x = x !! (i -1)

makeList :: [[Int]] -> Int -> [Int]
makeList (x : xs) i
  | length (x : xs) == 0 = []
  | length xs == 0 = [x !! i]
  | otherwise = x !! i : makeList xs i

getCol :: [[Int]] -> Int -> [Int]
getCol (x : xs) i
  | isMatrix (x : xs) && i -1 > length xs = []
  | isMatrix (x : xs) = makeList (x : xs) (i -1)

transposer :: [[Int]] -> Int -> [[Int]]
transposer (x : xs) i
  | i == 1 = [getCol (x : xs) (length x)]
  | otherwise = getCol (x : xs) (length x + 1 - i) : transposer (x : xs) (i -1)

trav :: [[Int]] -> [[Int]]
trav (x : xs)
  | not (isMatrix (x : xs)) = [[]]
  | otherwise = transposer (x : xs) (length x)

setInRow :: [Int] -> Int -> Int -> [Int]
setInRow [] _ _ = []
setInRow (x : xs) j a
  | j /= 1 = x : setInRow xs (j -1) a
  | j == 1 = a : setInRow xs (j -1) a
  | otherwise = []

setEntry :: [[Int]] -> Int -> Int -> Int -> [[Int]]
setEntry [] _ _ _ = []
setEntry (x : xs) i j a
  | isMatrix (x : xs) && i /= 1 = x : setEntry xs (i -1) j a
  | isMatrix (x : xs) && i == 1 = setInRow x j a : setEntry xs (i -1) j a
  | otherwise = []