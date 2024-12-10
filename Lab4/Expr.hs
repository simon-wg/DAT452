module Expr where

import Parsing (Parser, parse)

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
showExpr (Sin e) = "sin(" ++ showExpr e ++ ")"
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
exprParser

readExpr :: String -> Maybe Expr
readExpr = parse