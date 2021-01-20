isMatrix :: [[Int]] -> Bool
isMatrix [[]] = False 
isMatrix (x:xs) | length (x:xs) == 1 = True 
                | otherwise = length x == length (xs !! 0) && isMatrix xs 

spalten :: [[Int]] -> Int
spalten (x : xs) = length x

zeilen :: [[Int]] -> Int
zeilen x = length x

dimensions :: [[Int]] -> (Int, Int)
dimensions x | not(isMatrix x) = (-1,1)
             | otherwise = (zeilen x, spalten x)


isQuadratic :: [[Int]] -> Bool
isQuadratic x | not (isMatrix x) = False 
              | spalten x == zeilen x = True 
              | otherwise = False 

getRow :: [[Int]] -> Int -> [Int]
getRow xs y | not (isMatrix xs) = []
            | y > length xs = []
            | y < 0 = []
            | otherwise = xs !! (y-1) 
            
getCol :: [[Int]] -> Int -> [Int]
getCol (x:xs) y | not (isMatrix (x:xs)) = []
                | y > length x = []
                | y <= 0 = []
                | length xs == 0 = [x !! (y-1)]
                | otherwise = (x !! (y-1)) : getCol xs y

trav :: [[Int]] -> [[Int]]
trav (x:xs) | not (isMatrix (x:xs)) = [[]]
            | otherwise = travHelper (x:xs) (length x)

travHelper :: [[Int]] -> Int -> [[Int]]
travHelper (x:xs) y | y == 1 = [getCol (x:xs) (length x)]
                    | otherwise = getCol (x:xs) (length x + 1 - y) : travHelper (x:xs) (y - 1)

setEntry :: [[Int]] -> Int -> Int -> Int -> [[Int]]
setEntry [] _ _ _ = []
setEntry (x : xs) i j a | isMatrix (x : xs) && i /= 1 = x : setEntry xs (i -1) j a
                        | isMatrix (x : xs) && i == 1 = setInRow x j a : setEntry xs (i -1) j a
                        | otherwise = []

setInRow :: [Int] -> Int -> Int -> [Int]
setInRow [] _ _ = []
setInRow (x : xs) j a
  | j /= 1 = x : setInRow xs (j -1) a
  | j == 1 = a : setInRow xs (j -1) a
  | otherwise = []

-- prints the Matrix in a Col/Row format instead of a linear one

printasMatrix :: [[Int]] -> IO()
printasMatrix [x] = print x
printasMatrix (x:xs) | not (isMatrix (x:xs)) = print "this is not a Matrix"
                | otherwise =  do
                                print x
                                printasMatrix xs
