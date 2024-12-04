module Sudoku where

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
prop_blocks_lengths sud = and (map (\r -> length r == 9) (blockRowCol)) && length blockRowCol == 27
  where blockRowCol = blocks sud

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
blanks = undefined

-- prop_blanks_allBlanks :: ...
-- prop_blanks_allBlanks =

-- * E2

(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (i, y) = undefined

-- prop_bangBangEquals_correct :: ...
-- prop_bangBangEquals_correct =

-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

-- prop_update_updated :: ...
-- prop_update_updated =

------------------------------------------------------------------------------

-- * F1

-- * F2

-- * F3

-- * F4
