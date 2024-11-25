module Sudoku where

import Data.List (nub)
import Test.QuickCheck

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell

type Row = [Cell] -- a row is a list of cells

data Sudoku = Sudoku [Row]
  deriving (Show, Eq)

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

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
allBlankSudoku = Sudoku [[Nothing | _ <- [1 .. 9]] | _ <- [1 .. 9]]

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku = all validRow . rows
  where
    validRow row = length row == 9 && all validCell row
    validCell Nothing = True
    validCell (Just n) = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled = all filledRow . rows
  where
    filledRow = all filledCell
    filledCell Nothing = False
    filledCell (Just _) = True

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku = putAllRows . rows
  where
    putAllRows [] = return ()
    putAllRows (r : rs) = do
      putRow r
      putAllRows rs
    putRow [] = putStrLn ""
    putRow (c : cs) = do
      putCell c
      putRow cs
    putCell Nothing = putStr "."
    putCell (Just n) = putStr $ show n

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
isOkayBlock block = filter isJust block == nub (filter isJust block)
  where
    isJust (Just _) = True
    isJust Nothing = False

-- * D2

blocks :: Sudoku -> [Block]
blocks sud =
  [ [ rows' !! (r + i) !! (c + j)
      | i <- [0 .. 2],
        j <- [0 .. 2]
    ]
    | r <- [0, 3, 6],
      c <- [0, 3, 6]
  ]
  where
    rows' = rows sud

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length (blocks sud) == length (rows sud)

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (rows sud) && all isOkayBlock (blocks sud) && isSudoku sud

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
