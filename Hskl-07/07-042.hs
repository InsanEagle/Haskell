   -- Демонстрация оригами-программирования в
   --   fix-foldr-,  fix-foldl-,
   --   fix-unfold-, fix-foldr-unfold-стилях
   -- **************************************
   import List

   -- **************************************************
   -- Функция, моделирующая функционал unfold на списках
   -----------------------------------------------------
   unfold p f g x | p x  = [] 
                  | True = f x : unfold p f g (g x)

   -- ********************************************
   -- (1) fix-foldr-реализация вычисления значения
   --     нерекурсивной функции (\x -> x + 1) 
   ------------------------------------------
   inc = foldr f e [1..]
        where f = \_ -> g
              e = undefined
              g = \f x -> x + 1

   -- Выполним рефакторинг функции inc и получим
   -- следующее foldr-представление:

   inc' = foldr f e [1..]
        where f = \_ _ x -> x + 1
              e = undefined

   --------------------------------
   -- Выполним "ручные" вычисления:
   --
   --  inc' 3 = (foldr f e [1..]) 3 = f 1 (f 2 (f ...)) 3 =
   --         = (\_ _ x -> x+1) 1 (f 2 (f ...)) 3 =
   --         = \x -> x+1) 3 = 4

   -- ********************************************
   -- (2) fix-foldr-реализация функции "факториал" 
   -----------------------------------------------
   fact = foldr f e [1..]
        where f    = \_ -> fct
              e    = undefined
              fct = \f n -> if n==0 then 1 else n * f (n-1)

   -- Выполним рефакторинг функции fact и получим следующее
   -- foldr-представление:

   fact' = foldr f e [1..]
        where f = \_ f n -> if n==0 then 1 else n * f (n-1)
              e = undefined

   -- ********************************************
   -- (3) fix-foldl-реализация вычисления значения
   --     функции "факториал"
   ----------------------------
   fact'' = foldl f e [1..1000]
        where f        = \g _ -> fct' g
              e        = undefined
              fct' g n = if n==0 then 1 else n * g (n-1)

   -- Выполним рефакторинг функции fact'' и получим следующее
   -- foldl-представление:

   fact''' = foldl f e [1..10000]
        where f g _ n = if n==0 then 1 else n * g (n-1)
              e       = undefined

   -- *************************************
   -- (4) fix-foldl-реализация функции (++)
   -----------------------------------------
   -- Воспользуемся реализацией функции (++)
   -- из библиотеки List:
   --
   -- (++) []     y = y
   -- (++) (x:xs) y = x : (++~) xs y
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   -- (++~) h []     y = y             
   -- (++~) h (x:xs) y = x : h xs y
   --
   -- и создадим fix-foldl-представление:

   (++~) = foldl f e [1..10000]                      
        where f g _ []     z = z         
              f g _ (x:xs) z = x : g xs z
              e              = undefined

   -- *************************************
   -- (5) fix-foldr-реализация функции tail
   ----------------------------------------
   tail' = foldr f e (repeat undefined)
        where f = \_ -> g
              e = undefined 
              g = \f (_:xs) -> xs
  
   -- Выполним простейший рефакторинг функции f:
   --
   -- f = \_ -> g = \_ f (_:xs) -> xs = \_ _ (_:xs) -> xs.
   --
   -- В результате получится foldr-представление:

   tail'' = foldr f e (repeat undefined)
        where f = \_ _ (_:xs) -> xs
              e = undefined 

   -- *************************************
   -- (6) fix-foldr-реализация функции drop
   --
   --  drop 0 xs     = xs            
   --  drop _ []     = []            
   --  drop n (x:xs) = drop' (n-1) xs
   --
   -------------------------
   drop':: Int -> [a] -> [a]
   drop' = foldr f e [1..]
        where f = \_ -> drop
              e = undefined
              drop g n lst | n==0     = lst
                           | null lst = []
                           | True     = g (n-1) (tail lst)
  
   -- Выполним рефакторинг функции drop' и получим
   -- foldr-представление:

   drop'' = foldr f e [1..]
        where f _ _ 0 lst    = lst               
              f _ _ _ []     = []                
              f _ g n (_:xs) = g (n-1) xs
              e              = undefined

   -- ************************************
   -- (7) fix-foldl-реализация функции map
   ---------------------------------------
   map' = foldl f e [1..1000]
        where f = \x _ -> g x
              e = undefined
              g = \f h xs -> [h z | z <- xs]

   -- Выполним простейший рефакторинг функции map'
   -- и получим foldl-представление:

   map'' = foldl f e [1..1000]
        where f _ _ h xs = [h z | z <- xs]
              e          = undefined

   -- ******************************************
   -- (8) fix-foldr-реализация функции dropWhile
   -------------------------------------------------
   -- Воспользуемся реализацией функции dropWhile из
   -- библиотеки List:
   --
   --  dropWhile p []     = []
   --  dropWhile p (x:xs) = if p x
   --                         then dropWhile p xs
   --                         else x : xs
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --  dropWhile' f p []     = []
   --  dropWhile' f p (x:xs) = if p x
   --                            then f p xs
   --                            else x : xs
   --
   -- и создадим fix-foldr-представление:

   dropWhile' = foldr f e (repeat undefined)
        where f _ _ _ []     = []            
              f _ g p (x:xs) = if p x        
                                 then g p xs
                                 else x : xs 
              e              = undefined 

   -- **************************************
   -- (9) fix-foldr-реализация функции tails
   ---------------------------------------------
   -- Воспользуемся реализацией функции tails из
   -- библиотеки List:
   --
   --  tails []     = [[]]
   --  tails (x:xs) = (x : xs) : tails xs
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --   tails' f []     = [[]]
   --   tails' f (x:xs) = (x : xs) : f xs
   --
   -- и создадим fix-foldr-представление:

   tails' = foldr f e (repeat undefined)
        where f           = \_ -> g
              e           = undefined
              g f1 []     = [[]]
              g f1 (x:xs) = (x : xs) : f1 xs

   -- Далее, выполним простейший рефакторинг функции f
   -- и получим foldr-представление:

   tails'' = foldr f e (repeat undefined)
        where f _ _ []     = [[]]
              f _ g (x:xs) = (x : xs) : g xs
              e            = undefined 

   -- **************************************
   -- (10) fix-foldr-реализация функции init
   --------------------------------------------
   -- Воспользуемся реализацией функции init из
   -- библиотеки List:
   --
   --   init [x]    = []
   --   init (x:xs) = x : init' xs
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --   init' h [x]    = []
   --   init' h (x:xs) = x : h xs
   --
   -- и создадим fix-foldr-представление:

   init' = foldr f e (repeat undefined)
        where f _ _ [x]    = []      
              f _ h (x:xs) = x : h xs
              e            = undefined

   -- **************************************
   -- (11) fix-foldl-реализация функции init
   --------------------------------------------
   -- Воспользуемся реализацией функции init из
   -- библиотеки List:
   --
   --   init [x]    = []
   --   init (x:xs) = x : init' xs
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --   init' h [x]    = []
   --   init' h (x:xs) = x : h xs
   --
   -- и создадим fix-foldl-представление:

   init'' = foldl f e [1..1000]
        where f _ _ [_]    = []      
              f h _ (x:xs) = x : h xs
              e            = undefined

   -- ****************************************
   -- (12) fix-foldr-реализация функции delete
   --      из модуля List
   --------------------------------------------
   -- Воспользуемся реализацией функции delete:
   --
   -- delete :: Eq a => a -> [a] -> [a]
   -- delete _ []     = []
   -- delete x (y:ys) = if x==y 
   --                     then ys 
   --                     else y : delete x ys
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   -- delete' h _ []     = []
   -- delete' h x (y:ys) = if x==y 
   --                        then ys 
   --                        else y : h x ys
   --
   -- и создадим fix-foldr-представление:

   delete' :: Eq a => a -> [a] -> [a]
   delete' = foldr f e [1..]
        where f _ _ _ []     = []               
              f _ h x (y:ys) = if x==y
                                 then ys
                                 else y : h x ys
              e              = undefined

   -- ***************************************
   -- (13) fix-foldr-реализация функции scanr
   -------------------------------------------
   -- Воспользуемся реализацией функции scanr:
   --
   --  scanr :: (a -> b -> b) -> b -> [a] -> [b]
   --  scanr f q0 []     = [q0]
   --  scanr f q0 (x:xs) = f x q : qs
   --     where qs@(q:_) = scanr f q0 xs
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --  scanr' h f q0 []     = [q0]
   --  scanr' h f q0 (x:xs) = f x q : qs
   --     where qs@(q:_) = h f q0 xs
   --
   -- и создадим fix-foldr-представление:

   scanr' = foldr f e [1..]                          
        where f _ _ _  q0 []     = [q0]       
              f _ h f' q0 (x:xs) = f' x q : qs
                where qs@(q:_) = h f' q0 xs
              e                  = undefined

   -- ***************************************
   -- (14) fix-foldr-реализация функции scanl
   -------------------------------------------
   -- Воспользуемся реализацией функции scanl:
   --
   --  scanl :: (a -> b -> a) -> a -> [b] -> [a]
   --  scanl f q xs = q : (case xs of
   --                       []   -> []
   --                       x:xs -> scanl f (f q x) xs)
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --  scanl' h f q xs = q : (case xs of
   --                          []   -> []
   --                          x:xs -> h f (f q x) xs)
   --
   -- и создадим fix-foldr-представление:

   scanl'= foldr f e [1..]                          
        where f _ h f' q xs = q : (case xs of
                                    []   -> []
                                    x:xs -> h f' (f' q x) xs)
              e             = undefined

   -- ***************************************
   -- (15) fix-foldl-реализация функции foldr
   -------------------------------------------
   -- Воспользуемся реализацией функции foldr:
   --
   --  foldr :: (a -> b -> b) -> b -> [a] -> b
   --  foldr f e []     = e
   --  foldr f e (x:xs) = x `f` foldr f e xs
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --  foldr' h f e []     = e                 
   --  foldr' h f e (x:xs) = f x (h f e xs)
   --
   -- и создадим два варианта fix-foldl-представления:

   foldr' = foldl f e [1..500]                   
        where f _ _ _ e []     = e               
              f h _ g e (x:xs) = g x (h g e xs)
              e                = undefined
   ---------------------------------------------------------
   foldr'' f e lst = foldl f' e' [1..1 + length lst] f e lst
        where f' _ _ _ e' []     = e'
              f' h _ g e' (x:xs) = g x (h g e' xs)
              e'                 = undefined

   -- *****************************************
   -- (16) fix-foldr-реализация функции zipWith
   ---------------------------------------------
   -- Воспользуемся реализацией функции zipWith:
   --
   --  zipWith z (a:as) (b:bs) = z a b : zipWith z as bs
   --  zipWith _ _      _      = []
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --  zipWith' h z (a:as) (b:bs) = z a b : h z as bs
   --  zipWith' h _ _      _      = []
   --
   -- и создадим fix-foldr-представление:

   zipWith' = foldr f e [1..]                          
       where f _ h z (a:as) (b:bs) = z a b : h z as bs
             f _ _ _ _      _      = []               
             e                     = undefined

   -- *************************************
   -- (17) fix-foldl-реализация функции zip
   -----------------------------------------
   -- Воспользуемся реализацией функции zip:
   --
   --  zip :: [a] -> [b] -> [(a,b)]
   --  zip xs []         = []
   --  zip [] ys         = []
   --  zip (x:xs) (y:ys) = (x,y) : zip xs ys
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   --  zip' h xs     []     = []
   --  zip' h []     ys     = []
   --  zip' h (x:xs) (y:ys) = (x,y) : h xs ys
   --
   -- и создадим fix-foldl-представление:

   zip' l1 l2 = foldl f e [1..1+max (length l1) (length l2)] l1 l2                  
        where f _ _ _  []         = []               
              f _ _ [] _          = []              
              f h _ (x:xs) (y:ys) = (x,y) : h xs ys 
              e                   = undefined

   -- ****************************************
   -- (18) fix-foldl-реализация функции delete
   --      из модуля List
   --------------------------------------------
   -- Воспользуемся реализацией функции delete:
   --
   -- delete :: Eq a => a -> [a] -> [a]
   -- delete _ []     = []
   -- delete x (y:ys) = if x==y 
   --                     then ys 
   --                     else y : delete x ys
   --
   -- по которой построим нерекурсивную "заготовку"
   --
   -- delete' h _ []     = []
   -- delete' h x (y:ys) = if x==y 
   --                        then ys 
   --                        else y : h x ys
   --
   -- и создадим fix-foldl-представление:

   delete'' :: Eq a => a -> [a] -> [a]
   delete'' = foldl f e [1..1001]                   
       where f _ _ _ []     = []               
             f h _ x (y:ys) = if x==y          
                                then ys        
                                else y : h x ys
             e              = undefined

   -- **************************************
   -- (19) fix-unfold-реализация функции map
   --------------------------------------------------
   -- Воспользовавшись unfold-реализацией комбинатора
   -- неподвижной точки и нерекурсивной "заготовкой"
   --
   -- g = \f h xs -> [h z | z <- xs],
   --
   -- получим:

   map''' = last $ [e] ++ unfold p f' g' ([1..100],e)  
       where f  x _ h xs = [h z | z <- xs]
             e           = undefined
             p  (x,y)    = null x
             f' (x,y)    = f y (head x)
             g' (x,y)    = (tail x, f' (x,y))

   -- ********************************************
   -- (20) fix-foldr-unfold-реализация функции inc
   ----------------------------------------------------
   inc'' = (foldr f e . unfold p f1 g1) (\f x -> x + 1)
        where f  = ($)
              e  = undefined
              p  = \x -> False
              f1 = id
              g1 = id                     

   -- ********************************************
   -- (21) fix-foldr-unfold-реализация функции map
   -----------------------------------------------
   map'''' = (foldr f e . unfold p f1 g1)
                             (\f g xs -> [g x | x <- xs])      
        where f  = ($)
              e  = undefined
              p  = \x -> False
              f1 = id
              g1 = id                     

   -- ***************************
   -- Неудачные тестовые примеры:
   ------------------------------------------------------------
   test1  = inc' 111111111111111111111 == 111111111111111111112 
   test1' = inc'' 111111111111111111111 == 111111111111111111112 
   test2  = map' (\x -> fact x == fct x) [1000..1100]
       where fct n = if n==0 then 1 else n * fct (n-1)
   test3  = fact 3990 
   --------------------------------------------
   test4 = map' (\x -> drop' (fromInt x) lst == 
                       drop'' (fromInt x) lst) lst
       where lst = [0..50]
   test5 = take 10 $ drop' 5 [1..]         -- Важный пример!
   ---------------------------------------------------------
   test6 = map' (\x -> head x) [[1,2,3],[2,5],[3,7,8,9]] ==
           map'' (\x -> head x) [[1,2,3],[2,5],[3,7,8,9]]
   test7 = map''' (^2) [1..1000] == map'' (^2) [1..1000]
   test8 = map''   (\x -> (x, lst, delete x lst)) lst ==
           map'''' (\x -> (x, lst, delete x lst)) lst
       where lst = [1..90]
   test9 = map'''' (\x -> (x, lst, delete x lst)) lst ==
           map'''  (\x -> (x, lst, delete x lst)) lst
       where lst = [1..90]
   ---------------------------------------------
   test10 = dropWhile' (< 0) [-1,-2,-3,4,-5,3,2] 
              == dropWhile (< 0) [-1,-2,-3,4,-5,3,2] 
   test11 = dropWhile' (< 0) [4,-5,3,2] 
              == dropWhile (< 0) [4,-5,3,2] 
   test12 = ("123" ++~ "123", "" ++~ "")
   test13 = [1..5000] ++~ [1..5000]
   ---------------------------------------------------
   test14   = delete'  1 (replicate 1000 2 ++ [1,1,1])
   test14'  = delete'' 2 [1,1,1,5,4,2,6]
   test14'' = delete'' 1 z 
           where z = replicate 1000 2 ++ [1,1,1]
   ---------------------------------------------
   test15 = (init' [1], init' [1..4]) == 
            (init'' [1], init'' [1..4])
   --------------------------------------------------
   test16 = scanr' (+) 1 [1..5] == scanr (+) 1 [1..5]
   test17 =   scanr' (+) 0 [1,2,3,4,5] == [15,14,12,9,5,0]
           && scanr' (*) 2 [5,6,7]     == [420,84,14,2]
           && scanr' min (-5) [-1,2,-13,-4,-6] 
                                       == [-13,-13,-13,-6,-6,-5]
           && scanr' (-) 0 [1,2,3,4,5] == [3,-2,4,-1,5,0]
   ------------------------------------------------------
   test18 =   scanl' (+) 0 [1,2,3,4,5] == [0,1,3,6,10,15]
           && scanl' (*) 2 [5,6,7]     == [2,10,60,420]
           && scanl' min (-5) [-1,2,-13,-4,-6]
                                       == [-5,-5,-5,-13,-13,-13]
           && scanl' (-) 0 [1..5]      == [0,-1,-3,-6,-10,-15]
   -----------------------------------------------------------
   test19 = foldr' (+) 0 [1..100] == foldr (+) 0 [1..100]
   test20 = foldr' (*) 1 [1..60]  == foldr (*) 1 [1..60]
   test21 =   foldr' (++) [] [[1,2,3],[5,6,7]]
           == foldr  (++) [] [[1,2,3],[5,6,7]]
   test22 = foldr'' (+) 0 [1..2000] == foldr (+) 0 [1..2000]
   ---------------------------------------------------------
   test23 = zip' [1..10] [2..11]
   test24 = zipWith' (+) [1..5] [1..20] ==
            zipWith' (+) [1..5] [1..20]
