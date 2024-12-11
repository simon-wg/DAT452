module Expr where

import Data.Fixed (mod')
import Data.Maybe
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

size :: Expr -> Int
size (Num _) = 0
size X = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Sin e) = 1 + size e
size (Cos e) = 1 + size e

-- | Part B
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
eval :: Expr -> Double -> Double
eval X x = x
eval (Num n) _ = n
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Sin e) x = Prelude.sin (eval e x)
eval (Cos e) x = Prelude.cos (eval e x)

-- | Part D
numParser :: Parser Expr
numParser = do
  n <- readsP
  zeroOrMore (char ' ')
  return $ Num n

-- \| Num <$> readsP

expr, term, factor, sinExpr, cosExpr :: Parser Expr
expr = do
  t <- term
  ts <- zeroOrMore (do char '+'; term)
  return $ foldl Add t ts
term = do
  f <- factor
  fs <- zeroOrMore (do char '*'; factor)
  return $ foldl Mul f fs
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
prop_ShowReadExpr e = round (eval e 1) == round (eval e' 1)
  where
    Just e' = readExpr (showExpr e)

rNum :: Gen Expr
rNum = do
  n <- choose (0.0, 9.0)
  return $ Num n

arbExpr :: Int -> Gen Expr
arbExpr depth = do
  n <- choose (0, depth)
  case n of
    0 -> rNum
    _ -> do
      e1 <- arbExpr (depth - 1)
      e2 <- arbExpr (depth - 1)
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
simplify :: Expr -> Expr
simplify (Add X X) = Mul (Num 2) X
simplify (Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify (Add (Num 0) e) = simplify e
simplify (Add e (Num 0)) = simplify e
simplify (Mul X (Mul (Num n1) X)) = simplify $ Mul (Num n1) (Mul X X)
simplify (Mul (Mul (Num n1) X) X) = simplify $ Mul (Num n1) (Mul X X)
simplify (Mul (Mul X X) (Num n1)) = simplify $ Mul (Num n1) (Mul X X)
simplify (Mul (Num n1) (Num n2)) = Num (n1 * n2)
simplify (Mul (Num 0) _) = Num 0
simplify (Mul _ (Num 0)) = Num 0
simplify (Mul (Num 1) e) = simplify e
simplify (Mul e (Num 1)) = simplify e
simplify (Mul e1 e2) = Mul (simplify e1) (simplify e2)
simplify (Sin (Num n)) = Num (Prelude.sin n)
simplify (Sin e) = Sin (simplify e)
simplify (Cos (Num n)) = Num (Prelude.cos n)
simplify (Cos e) = Cos (simplify e)
simplify e = e

prop_simplify :: Expr -> Bool
prop_simplify e = eval e 1 == eval (simplify e) 1

-- | Part G
differentiate :: Expr -> Expr
differentiate (Num _) = Num 0
differentiate X = Num 1
differentiate (Add e1 e2) = simplify $ Add (differentiate e1) (differentiate e2)
differentiate (Mul e1 e2) = simplify $ Add (simplify $ Mul e1 (differentiate e2)) (simplify $ Mul e2 (differentiate e1))
differentiate (Sin e) = simplify $ Mul (Cos e) (differentiate e)
differentiate (Cos e) = simplify $ Mul (Mul (Num (-1)) (Sin e)) (differentiate e)
