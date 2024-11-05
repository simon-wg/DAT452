module Lab1 where
{- Lab 1
   Date: 5/11/2024
   Authors: 
   Lab group: 
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n 0  = 1
power n k | k < 0     = error "power: negative argument"
          | otherwise = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1

-- B -------------------------
-- power1

power1 n k = product (replicate (fromInteger k) n)

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | even k     = power2 n (div k 2) * power2 n (div k 2)
           | otherwise  = n * power2 n (k-1)


-- D -------------------------
{- 

<Describe your test cases here>
  [(1,0), (2,2), (2,-1), (12,12)]

 -}

-- 
prop_powers n k = power n k == power1 n k && power1 n k == power2 n k

--
powerTest :: Bool
powerTest = prop_powers 1 0 && prop_powers 2 2 && prop_powers 2 (-1) && prop_powers 12 12

--
prop_powers' = undefined