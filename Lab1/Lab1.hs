{- Lab 1
   Date: 5/11/2024
   Authors:
   Lab group:
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

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product (replicate (fromIntegral k) n)

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k
  | k < 0 = error "power: negative argument"
  | even k = power2 n (div k 2) * power2 n (div k 2)
  | otherwise = n * power2 n (k - 1)

-- D -------------------------
{-

<Describe your test cases here>
  [(1,0), (2,2), (2, 2^20), (12,12)]
 -}

--
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power1 n k == power2 n k

--
test_cases = [(1, 0), (2, 2), (2, 2 ^ 20), (12, 12)]

powerTest = and [prop_powers n k | (n, k) <- test_cases]

--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k =
  power n (abs k) == power1 n (abs k)
    && power1 n (abs k) == power2 n (abs k)