   import List

   -- Довжиков С.
   --------------------------------------
   task = filter (\x -> not $ isPower2 x)
      where isPower2 x | x == 1         = True
                       | x `mod` 2 == 1 = False
                       | True           = isPower2 (x `div` 2)

   -- Кальницкий В.
   -----------------
   wout2n n l@(x:xs) 
      | l  == []           = []
      | xs == [] && x == n = []
      | xs == [] && x /= n = [x]
      | x  == n            = wout2n (n * 2) xs
      | True               = x : wout2n n xs

   -- Швецкий М.В.
   -------------------
   abc :: Int -> [Int]
   abc n = take n $ (\\) [1..n] (map (2^) [1..z])
              where z = floor $ logBase 2.0 (fromInt n)

   ----------------------------------
   test1 = last $ wout2n 2 [1..100000]
   test2 = last $ [1] ++ task [1..100000]
   test3 = last $ abc 100000
