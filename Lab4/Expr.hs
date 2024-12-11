module Expr where

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
  e <- (do char ' '; e <- factor; return e) <|> factor
  return $ Sin e
cosExpr = do
  char 'c'
  char 'o'
  char 's'
  e <- (do char ' '; e <- factor; return e) <|> factor
  return $ Cos e
factor =
  numParser
    <|> (do char '('; e <- expr; char ')'; return e)
    <|> (do char 'x'; return X)
    <|> sinExpr
    <|> cosExpr

readExpr :: String -> Maybe Expr
readExpr s = Just e
  where
    Just (e, l) = parse expr s

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = (eval (fromJust $ readExpr $ showExpr e) 0) == eval e 0

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