module Expr where

import Parsing
import Test.QuickCheck

data Expr
  = Num Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
  | X
  deriving (Eq, Show)

-- | Part A
x :: Expr
x = X

num :: Double -> Expr
num = Num

add :: Expr -> Expr -> Expr
add = Add

mul :: Expr -> Expr -> Expr
mul = Mul

sin :: Expr -> Expr
sin = Sin

cos :: Expr -> Expr
cos = Cos

-- | Simple recursive function to calculate the amount of nodes in an
-- | expression tree.
size :: Expr -> Int
size (Num _) = 0
size X = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Sin e) = 1 + size e
size (Cos e) = 1 + size e

-- | Part B
-- | Here we thought about every possible situation we could come up with
-- | and used pattern matching to print all the different cases.
showExpr :: Expr -> String
showExpr (Num x) = show x
showExpr X = "x"
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
  where
    showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
    showFactor e = showExpr e
showExpr (Sin X) = "sin x"
showExpr (Sin (Num i)) = "sin " ++ show i
showExpr (Sin e) = "sin(" ++ showExpr e ++ ")"
showExpr (Cos X) = "cos x"
showExpr (Cos (Num i)) = "cos " ++ show i
showExpr (Cos e) = "cos(" ++ showExpr e ++ ")"

-- | Part C
-- | Here we used pattern matching to evaluate the expression tree.
eval :: Expr -> Double -> Double
eval X x = x
eval (Num n) _ = n
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Sin e) x = Prelude.sin (eval e x)
eval (Cos e) x = Prelude.cos (eval e x)

-- | Part D
-- | We use the parser from the Parsing library to parse the expression.
-- | Before any parsing occurrs we strip all whitespace from the input string.
numParser :: Parser Expr
numParser = do
  n <- readsP
  zeroOrMore (char ' ')
  return $ Num n

-- \| Num <$> readsP

expr, term, factor, sinExpr, cosExpr :: Parser Expr

-- | In order we check
-- | 1. If the expression is a term
-- | 2. If it is a term followed by a '+' and another term
-- | 3. If it is a term followed by a '*' and another term
-- | 4. If it is a sin expression
-- | 5. If it is a cos expression
-- | 6. If it is a number
-- | These are in order of precedence.
expr = do
  t <- term
  ts <- zeroOrMore (do char '+'; term)
  return $ foldr Add t ts

term = do
  f <- factor
  fs <- zeroOrMore (do char '*'; factor)
  return $ foldr Mul f fs

sinExpr = do
  char 's'
  char 'i'
  char 'n'
  Sin <$> factor

cosExpr = do
  char 'c'
  char 'o'
  char 's'
  Cos <$> factor

factor =
  (do char '('; e <- expr; char ')'; return e)
    <|> (do char 'x'; return X)
    <|> sinExpr
    <|> cosExpr
    <|> numParser

readExpr :: String -> Maybe Expr
readExpr s = Just e
  where
    Just (e, l) = parse expr (filter (/= ' ') s)

-- | Part E

{-
We noticed after iterating a few different rounding points, that we got deeper
trees the less precise the rounding was. To us this indicates that the problem
lies in floating point imprecision.
-}
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = round (1e10 * eval e 1) == round (1e10 * eval e' 1)
  where
    Just e' = readExpr (showExpr e)

rNum :: Gen Expr
rNum = do
  n <- choose (0.0, 9.0)
  return $ Num n

-- | Our arbitrary expression has a depth which prevents it from
-- | becoming too large.
arbExpr :: Int -> Gen Expr
arbExpr depth = do
  n <- choose (0, depth)
  case n of
    0 -> rNum
    _ -> do
      e1 <- arbExpr (depth `div` 2)
      e2 <- arbExpr (depth `div` 2)
      oneof
        [ rNum,
          return e1,
          return e2,
          return $ Add e1 e2,
          return $ Mul e1 e2,
          return $ Sin e1,
          return $ Cos e1
        ]

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- | Part F
-- | We use pattern matching for every case we could think of to simplify
-- | the expression.
simplify :: Expr -> Expr
simplify (Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify (Add (Num 0) e) = simplify e
simplify (Add e (Num 0)) = simplify e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify (Mul (Num n1) (Num n2)) = Num (n1 * n2)
simplify (Mul (Num 0) _) = Num 0
simplify (Mul _ (Num 0)) = Num 0
simplify (Mul (Num 1) e) = simplify e
simplify (Mul e (Num 1)) = simplify e
simplify (Mul e1 e2) = Mul (simplify e1) (simplify e2)
simplify (Sin (Num n)) = Num (Prelude.sin n)
simplify (Sin X) = Sin X
simplify (Sin (Add e1 e2)) = Sin (simplify $ Add e1 e2)
simplify (Sin (Mul e1 e2)) = Sin (simplify $ Mul e1 e2)
simplify (Sin e) = simplify $ Sin (simplify e)
simplify (Cos (Num n)) = Num (Prelude.cos n)
simplify (Cos X) = Cos X
simplify (Cos (Add e1 e2)) = Cos (simplify $ Add e1 e2)
simplify (Cos (Mul e1 e2)) = Cos (simplify $ Mul e1 e2)
simplify (Cos e) = simplify $ Cos (simplify e)
simplify e = e

prop_simplify :: Expr -> Bool
prop_simplify e = eval e 1 == eval (simplify e) 1

-- | Part G
-- | We use pattern matching to differentiate the expression.
-- | We also simplify the expression after differentiating it.
differentiate :: Expr -> Expr
differentiate (Num _) = Num 0
differentiate X = Num 1
differentiate (Add e1 e2) =
  simplify $
    Add (differentiate e1) (differentiate e2)
differentiate (Mul e1 e2) =
  simplify $
    Add
      ( simplify $
          Mul e1 (differentiate e2)
      )
      ( simplify $
          Mul e2 (differentiate e1)
      )
differentiate (Sin e) =
  simplify $ Mul (Cos e) (differentiate e)
differentiate (Cos e) =
  simplify $ Mul (Mul (Num (-1)) (Sin e)) (differentiate e)
