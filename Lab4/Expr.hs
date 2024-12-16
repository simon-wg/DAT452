module Expr where

import Data.Maybe
import Parsing
import Test.QuickCheck

-- Part A ##################

-- | Class describing mathematical expressions
-- | A thing of type expression is either:
-- | A number of type double
-- | An Operation of two expressions
-- | A function of one expression
-- | The variable X
data Expr = Num Double | Optr Op Expr Expr | Func Fn Expr | X
  deriving (Eq, Show)

-- | Class describing Operations
-- | Currently contain addition and multiplication
data Op = Add | Mul
  deriving (Eq, Show)

-- | Class describing Functions
-- | Currently contains Sin and Cos
data Fn = Sin | Cos
  deriving (Eq, Show)

x :: Expr
x = X

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Optr Add
mul = Optr Mul

sin,cos :: Expr -> Expr
sin = Func Sin
cos = Func Cos

-- | Function counting the sum of the number of operations
-- | and functions in an expression. Result is an int.
size :: Expr -> Int
size X = 0
size (Num _) = 0
size (Optr op e1 e2) = 1 + size e1 + size e2
size (Func fn e) = 1 + size e

-- Part B ##################

-- | Remakes operations into strings for prettier formating
showExpr :: Expr -> String
showExpr X = "x"
showExpr (Num d) = show d
showExpr (Optr op e1 e2) = showOp op e1 e2
showExpr (Func fn e) = showFn fn e

-- | Re-formats an operation into a string
showOp :: Op -> Expr -> Expr -> String
showOp Add e1 e2 = showExpr e1 ++ " + " ++ showExpr e2
showOp Mul e1 e2 = showFactor e1 ++ " * " ++ showFactor e2
  where
    showFactor (Optr Add e1 e2) = "(" ++ showExpr (Optr Add e1 e2) ++ ")"
    showFactor e = showExpr e

-- | Re-formats a function into a string
showFn :: Fn -> Expr -> String
showFn Sin X = "sin x"
showFn Cos X = "cos x"
showFn Sin (Num d) = "sin " ++ show d
showFn Cos (Num d) = "cos " ++ show d
showFn Sin e = "sin(" ++ showExpr e ++ ")"
showFn Cos e = "cos(" ++ showExpr e ++ ")"

-- Part C #################################

-- | When given an operation returns the associated mathematical operation
getOp :: Op -> (Double -> Double -> Double)
getOp Add = (+)
getOp Mul = (*)

-- | When given a function returns the associated mathematical function
getFn :: Fn -> (Double -> Double)
getFn Sin = Prelude.sin
getFn Cos = Prelude.cos

-- | Takes an expression and a value for X and evaluates the expression
-- | using the value for X when applicable.
eval :: Expr -> Double -> Double
eval X x = x
eval (Num n) _ = n
eval (Optr op e1 e2) x = (getOp op) (eval e1 x) (eval e2 x)
eval (Func fn e) x = (getFn fn) (eval e x)

-- Part D #####################################3

-- | Function that when given two expressions combines them
-- | into an add operation
foldAdd :: Expr -> Expr -> Expr
foldAdd e1 e2 = Optr Add e1 e2

-- | Function when given two expressions combines them
-- | into a multiplication operation
foldMul :: Expr -> Expr -> Expr
foldMul e1 e2 = Optr Mul e1 e2

-- | Parses an expression by trying to find a number with a possible . character
numParser :: Parser Expr
numParser = do
  n <- readsP
  zeroOrMore (char ' ')
  return $ Num n

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
  return $ foldl foldAdd t ts

term = do
  f <- factor
  fs <- zeroOrMore (do char '*'; factor)
  return $ foldl foldMul f fs

sinExpr = do
  char 's'
  char 'i'
  char 'n'
  Func Sin <$> factor

cosExpr = do
  char 'c'
  char 'o'
  char 's'
  Func Cos <$> factor

factor =
  (do char '('; e <- expr; char ')'; return e)
    <|> (do char 'x'; return X)
    <|> sinExpr
    <|> cosExpr
    <|> numParser

-- | Reads a string looking for an exeption, returns Nothing if none is found
readExpr :: String -> Maybe Expr
readExpr s = Just e
  where
    Just (e, l) = parse expr (filter (/= ' ') s)

-- Part E #####################################

-- | Assures associativity within the expression
-- | Example: (1+2)+3 is considered equal to 1+(2+3)
assoc :: Expr -> Expr
assoc (Optr Add (Optr Add e1 e2) e3) = assoc (Optr Add e1 (Optr Add e2 e3))
assoc (Optr Add e1 e2) = Optr Add (assoc e1) (assoc e2)
assoc (Optr Mul (Optr Mul e1 e2) e3) = assoc (Optr Mul e1 (Optr Mul e2 e3))
assoc (Optr Mul e1 e2) = Optr Mul (assoc e1) (assoc e2)
assoc (Func Sin e) = Func Sin (assoc e)
assoc (Func Cos e) = Func Cos (assoc e)
assoc e = e

-- | Prop ensuring that an expression is the same
-- | before and after show and read
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = assoc e == assoc (fromJust $ readExpr $ showExpr e)

-- | Generator for a random double in the range 0 to 9
rNum :: Gen Expr
rNum = do
  n <- arbitrary
  return $ Num n

-- | Generator for expressions that gets smaller the deeper you go into the expression
-- | ensuring that the expression does not continue forever
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
          return $ Optr Add e1 e2,
          return $ Optr Mul e1 e2,
          return $ Func Sin e1,
          return $ Func Cos e1,
          return $ X
        ]

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- Part F #################################################

-- | Function that simplifies an expression by pattern matching
simplify :: Expr -> Expr
simplify e = simplifyHelper e 0

simplifyHelper :: Expr -> Int -> Expr
simplifyHelper (Num n) c                    = Num n
simplifyHelper (Optr Add (Num 0) e1) c      = simplifyHelper e1 0
simplifyHelper (Optr Add e1 (Num 0)) c      = simplifyHelper e1 0
simplifyHelper (Optr Mul (Num 0) _) c       = Num 0
simplifyHelper (Optr Mul _ (Num 0)) c       = Num 0
simplifyHelper (Optr Mul (Num 1) e) c       = simplifyHelper e 0
simplifyHelper (Optr Mul e (Num 1)) c       = simplifyHelper e 0
simplifyHelper X c                          = X
simplifyHelper (Optr op (Num n) (Num m))  c = Num ((getOp op) n m)
simplifyHelper (Func fn (Num n)) c          = Num ((getFn fn) n)
simplifyHelper (Optr op e1 e2) c 
    |c == 1    = Optr op (simplifyHelper e1 0) (simplifyHelper e2 0)
    |otherwise = simplifyHelper (Optr op (simplifyHelper e1 0) (simplifyHelper e2 0)) (c+1)
simplifyHelper (Func fn e) c 
    |c == 1    = Func fn (simplifyHelper e 0)
    |otherwise = simplifyHelper (Func fn (simplifyHelper e 0)) (c+1)

-- | Prop assuring that the value for an expression is the same
-- | before and after simplifying
prop_simplify :: Expr -> Bool
prop_simplify e = eval e 1 == eval (simplify e) 1

prop_multiple_simplify :: Expr -> Bool
prop_multiple_simplify e = simplify (simplify e) == simplify e

prop_simplify_simplifies :: Expr -> Bool
prop_simplify_simplifies e = check_no_junk $ simplify e

check_no_junk :: Expr -> Bool
check_no_junk (Optr op X e)             = check_no_junk e
check_no_junk (Optr op e X)             = check_no_junk e

check_no_junk (Optr Add (Num 0) e)      = False
check_no_junk (Optr Add e (Num 0))      = False
check_no_junk (Optr Mul (Num 0) e)      = False
check_no_junk (Optr Mul e (Num 0))      = False
check_no_junk (Optr Mul (Num 1) e)      = False
check_no_junk (Optr Mul e (Num 1))      = False
check_no_junk (Optr op (Num n) (Num m)) = False
check_no_junk (Optr op e1 e2)           = check_no_junk e1 && check_no_junk e2

check_no_junk (Func fn (Num n))         = False
check_no_junk (Func fn e)               = check_no_junk e

check_no_junk (Num n)                   = True
check_no_junk X                         = True

-- Part G ###############################################

-- | Differentiates a function by pattern matching
differentiate :: Expr -> Expr
differentiate (Num _) = Num 0
differentiate X = Num 1
differentiate (Optr Add e1 e2) = simplify
  (Optr
    Add
    (differentiate e1)
    (differentiate e2))
differentiate (Optr Mul e1 e2) = simplify
  (Optr
    Add
    (Optr Mul (differentiate e1) e2)
    (Optr Mul e1 (differentiate e2)))
differentiate (Func Sin e) = simplify $ Optr Mul (differentiate e) (Func Cos e)
differentiate (Func Cos e) = simplify $ Optr Mul (Num (-1)) (Optr Mul (differentiate e) (Func Sin e))
