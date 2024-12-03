import Data.List (nub, transpose)
import Data.Maybe
import Test.QuickCheck

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell

type Row = [Cell] -- a row is a list of cells

data Sudoku = Sudoku [Row]
  deriving (Show, Eq)

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

cols :: Sudoku -> [Row]
cols = transpose . rows

-- | A sample sudoku puzzle
example :: Sudoku
example =
  Sudoku
    [ [j 3, j 6, n, n, j 7, j 1, j 2, n, n],
      [n, j 5, n, n, n, n, j 1, j 8, n],
      [n, n, j 9, j 2, n, j 4, j 7, n, n],
      [n, n, n, n, j 1, j 3, n, j 2, j 8],
      [j 4, n, n, j 5, n, j 2, n, n, j 9],
      [j 2, j 7, n, j 4, j 6, n, n, n, n],
      [n, n, j 5, j 3, n, j 8, j 9, n, n],
      [n, j 8, j 3, n, n, n, n, j 6, n],
      [n, n, j 7, j 6, j 9, n, n, j 4, j 3]
    ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudoku =
  length (cols sudoku) == 9
    && all (all (\i -> i >= 1 && i <= 9) . catMaybes) (rows sudoku)
    && length (rows sudoku) == 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku []) = True
isFilled sud = isSudoku sud && all ((== 9) . length . catMaybes) (rows sud)

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do putStr $ buildString $ rows sudoku

buildString :: [Row] -> String
buildString = concatMap helper

helper :: [Maybe Int] -> String
helper [] = "\n"
helper ((Just i) : cs) = show i ++ helper cs
helper (Nothing : cs) = "." ++ helper cs

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp =
  do
    content <- readFile fp
    let sudoku = Sudoku $ [map readCell row | row <- lines content]
    if isSudoku sudoku
      then return sudoku
      else error "Invalid Sudoku"
  where
    readCell '.' = Nothing
    readCell c = Just (read [c])

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = do
  n <- elements [1 .. 9]
  frequency [(1, return $ Just n), (9, return Nothing)]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- vectorOf 9 (vectorOf 9 cell)
    return $ Sudoku rows

-- hint: get to know the QuickCheck function vectorOf

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-- hint: this definition is simple!

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = catMaybes block == nub (catMaybes block)

-- * D2

blocks :: Sudoku -> [Block]
blocks sud =
  rows sud
    ++ cols sud
    ++ realBlocks sud

realBlocks :: Sudoku -> [Block]
realBlocks sud =
  [ [ rows sud
        !! (i + a)
        !! (j + b)
      | i <- [0 .. 2],
        j <- [0 .. 2]
    ]
    | a <- [0, 3, 6],
      b <- [0, 3, 6]
  ]

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud =
  length (rows sud) == 9
    && length (cols sud) == 9
    && length (realBlocks sud) == 9

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud =
  all isOkayBlock (blocks sud)
    && isSudoku sud

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------

-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int, Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks sud = func 0 (rows sud)
  where
    func _ [] = []
    func i (r : rs) = blanksRow (i, 0) r ++ func (i + 1) rs

blanksRow :: (Int, Int) -> [Cell] -> [(Int, Int)]
blanksRow _ [] = []
blanksRow (row, col) (c : cs)
  | isFilled = blanksRow (row, col + 1) cs
  | otherwise = (row, col) : blanksRow (row, col + 1) cs
  where
    isFilled = isJust c

-- | This is a list of all combinations (x,y) where x and y are 0..8

{-
[(0,0),(0,2)...(0.8)]
[(1,0),(1,2)...(1.8)]
[...................]
[(8,0),(8,2)...(8.8)]
-}
blankPositions :: [Pos]
blankPositions = [(x, y) | x <- [0 .. 8], y <- [0 .. 8]]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = blanks allBlankSudoku == blankPositions

-- * E2

(!!=) :: [a] -> (Int, a) -> [a]
[] !!= _ = error "Index out of bounds"
(x : xs) !!= (i, y)
  | i < 0 = error "Index must be 0 or greater"
  | i >= length (x : xs) = error "Index greater than length of list - 1"
  | i == 0 = y : xs
  | i > 0 = x : (xs !!= (i - 1, y))

-- | Assures that given a list of ints and an element to put into that
-- | list at index i replaces the element at index i with the new value
prop_bangBangEquals_correct :: [Int] -> (Int, Int) -> Property
prop_bangBangEquals_correct putIn (i, new) = i >= 0 ==> length putIn > i ==> putIn !!= (i, new) !! i == new

-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update sud pos cell = Sudoku $ func 0 (rows sud) pos cell
  where
    func acc (r : rs) (row, col) cell
      | row < 0 || col < 0 = error "Invalid position for update: Indexes cannot be negative!"
      | row == acc = (r !!= (col, cell)) : rs
      | otherwise = r : func (acc + 1) rs (row, col) cell

prop_update_updated :: Sudoku -> Bool
prop_update_updated s1 = update s1 (0, 0) (Just 1) /= s1 || head (head (rows s1)) == Just 1

------------------------------------------------------------------------------

-- * F1

solve :: Sudoku -> Maybe Sudoku
solve sud
  | null solutions = Nothing
  | otherwise = Just $ head solutions
  where
    solutions = solve' (blanks sud) sud

solve' :: [Pos] -> Sudoku -> [Sudoku]
solve' emptyCells sud
  | not (isOkay sud && isSudoku sud) = []
  | null emptyCells = [sud]
  | otherwise = concatMap (solve' (tail emptyCells)) newList
  where
    newList = [update sud (head emptyCells) (Just i) | i <- [1 .. 9]]

-- * F2

readAndSolve :: FilePath -> IO ()
readAndSolve fp =
  do
    sud <- readSudoku fp
    let solved = solve sud
    maybe (putStr "(no solution)\n") printSudoku solved

-- * F3

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = allRowsSameOrJust (rows s1) (rows s2)
  where
    allRowsSameOrJust [] [] = True
    allRowsSameOrJust (r1 : rs1) (r2 : rs2)
      | allCellsSameOrJust r1 r2 = allRowsSameOrJust rs1 rs2
      | otherwise = False
    allCellsSameOrJust [] [] = True
    allCellsSameOrJust (c1 : cs1) (c2 : cs2)
      | c1 == c2 = allCellsSameOrJust cs1 cs2
      | isNothing c2 = allCellsSameOrJust cs1 cs2
      | otherwise = False

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isOkay s ==> isSolutionOf (fromJust (solve s)) s

fewerChecks prop = quickCheckWith stdArgs {maxSuccess = 30} prop

main :: IO ()
main = do
  quickCheck prop_Sudoku
  quickCheck prop_blocks_lengths
  quickCheck prop_blanks_allBlanks
  quickCheck prop_bangBangEquals_correct
  quickCheck prop_update_updated
  fewerChecks prop_SolveSound
  putStrLn "All tests passed!"