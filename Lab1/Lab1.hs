{- Lab 1
   Date: 6/11/2024
   Authors: Simon Westlin Green, Adam Hellgård
   Lab group: ??
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n 0 = 1
power n k
  | k < 0 = error "power: negative argument"
  | otherwise = n * power n (k - 1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1

-- B -------------------------
-- power1

{-
  First we forgot to check the base case of k = 0, but for some reason it works
  without the basecase. After checking on hoogle we found that product used on 
  an empty list returns 1.

  Therefore we don't need to check for k = 0.
-}

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product (replicate (fromIntegral k) n)

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k
  | k < 0 = error "power: negative argument"
  | even k = power2 (n * n) (div k 2)
  | otherwise = n * power2 n (k - 1)

-- D -------------------------
{-

<Describe your test cases here>
  [(2, 0), (0, 2), (3, 6), (4, 3), (-1, 1), (-1,2)]
  We selected one very large number, some even, some odd in order to test
  what happens e.g. when using replicate with large numbers, and power2 with
  both even and odds.
 -}

--
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power1 n k == power2 n k

--
test_cases :: [(Integer, Integer)]
test_cases = [(2, 0), (0, 2), (3, 6), (4, 3), (-1, 1), (-1, 2)]

powerTest :: Bool
powerTest = and [prop_powers n k | (n, k) <- test_cases]

--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k =
  power n (abs k) == power1 n (abs k)
    && power1 n (abs k) == power2 n (abs k)