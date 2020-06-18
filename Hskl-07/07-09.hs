   -- Демонстрация некоторых списочных гомоморфизмов,
   -- реализованных в foldr-, foldl- и foldl1-стилях
   -- **********************************************
   import Prelude hiding 
             (sum, product, elem, head, last, length,
              reverse, concat, and, or,
              maximum, minimum,
              map, concatMap, filter, any, all)
   --------------------------------------------
   id'  = foldr (:)                []
   id'' = foldl (\x y -> x ++ [y]) []
   ----------------------------------
   comp  = foldr (.)        id
   comp' = foldl (flip (.)) id
   ---------------------------
   sum  = foldr (+) 0
   sum' = foldl (+) 0
   ----------------------
   product  = foldr (*) 1
   product' = foldl (*) 1
   ----------------------
   elem x  = foldr f e
        where f = \z y -> if z==x then True else y
              e = False
   elem' x = foldl f e
        where f = \z y -> if y==x then True else z
              e = False
   ------------------------------
   head  = foldr  const undefined
   head' = foldl1 const
   -----------------------------------------
   -- Неэффективная реализация (два прохода)
   -----------------------------------------
   head'' :: (Num a, Ord a) => [a] -> a
   head'' lst = foldl f e lst
        where f = \x y -> if x==e then y else const x y
              e = 1 + maximum lst  -- "Почти" undefined
   ----------------------------------------------------
   last lst = foldr f e lst
        where f = \x y -> if y==e then x else const y x
              e = 1 + maximum lst  -- "Почти" undefined
   last' = foldl (\x y -> y) undefined
   -----------------------------------
   length  = foldr (\_ x -> 1 + x) 0
   length' = foldl (\x _ -> x + 1) 0
   ---------------------------------------
   reverse  = foldr (\x xs -> xs ++ [x]) []
   reverse' = foldl (\xs x -> x : xs)    []
   ----------------------------------------
   concat  = foldr (++) []
   concat' = foldl (++) []
   -----------------------
   and  = foldr (&&) True
   and' = foldl (&&) True
   ----------------------
   or  = foldr (||) False
   or' = foldl (||) False
   ----------------------------
   maximum :: Ord a => [a] -> a
   maximum  = foldr1 max
   -----------------------------
   maximum' :: Ord a => [a] -> a
   maximum' = foldl1 max
   ----------------------------
   minimum :: Ord a => [a] -> a
   minimum  = foldr1 min
   -----------------------------
   minimum' :: Ord a => [a] -> a
   minimum' = foldl1 min
   --------------------------------------
   map  g = foldr ((:) . g)            []
   map' g = foldl (\x y -> x ++ [g y]) []
   --------------------------------------
   concatMap g = foldr f e
        where f = (++) . g
              e = []
   concatMap' g = foldl f e
        where f = \x y -> x ++ g y 
              e = []
   -------------------------------------------------------
   filter  g = foldr (\x xs -> if g x then x : xs else xs)
                     []
   filter' g = foldl (\xs x -> if g x then xs ++ [x] else xs)
                     []
   --------------------
   all  p = foldr f e
         where f = \x y -> p x && y
               e = True
   all' p = foldl f e
         where f = \x y -> x && p y
               e = True
   --------------------
   any  p = foldr f e
         where f = \x y -> p x || y
               e = False
   any' p = foldl f e
         where f = \x y -> x || p y
               e = False

   -- ***************************
   -- Неудачные тестовые примеры:
   ----------------------------------
   test1  = (id' [1..5], id'' [1..5])
   test2  = (sum [1..5], sum' [1..5])
   test3  = (product  [1..5], product' [1..5])
   -------------------------------------------
   test4  = (head [1], head' [1], head'' [1])
   test5  = (last  [1..123], last' [1..567])
   test6  = (length  [1..123], length' [1..567])
   ---------------------------------------------
   test7  = concat  [[1,2],[3,4,5],[6,7,8,9]]
   test8  = concat  [[],[3,4,5],[]]
   test9  = concat' [[1,2],[3,4,5],[6,7,8,9]]
   test10 = concat' [[],[3,4,5],[]]
   --------------------------------
   test11 = and [True,True,True]
   test12 = or  [False,False,False,True]
   ----------------------------------------------
   test13 = (maximum [1..100], maximum' [1..100])
   test14 = (minimum  (reverse [1..100]),
             minimum' (reverse [1..100]))
   ----------------------------------------------
   test15 = (map (+ 1) [1..5], map' (+ 1) [1..5])
   test16 = (filter  (< 0) [1,-2,3,-4],
             filter' (< 0) [1,-2,3,-4])
   ---------------------------------------------------
   test17 = (all (<0) [-1..5],      all' (<0) [-1..5])
   test18 = (any (>0) [-1,-2,-3,4], any' (>0) [-1,-2,-3,4])
   ---------------------------------------------------------
   test19 =   elem' 3 [1..6] == elem 3 [1..6]
           && elem' 5 [1..4] == elem 5 [1..4]
   --------------------------------------------------------------
   test20 = comp' [asin,sin] (pi/2) == pi/2       -- Не попались!
   test21 = comp' [logBase (exp 1), exp] pi - pi  -- Попались!
   test22 = comp' [y (\f x -> x + 1),
                   y (\f x -> if x==0 then 1 else x * f (x-1))] 80
         where y f = f (y f)
