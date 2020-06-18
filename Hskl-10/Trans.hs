   -- Тестирование некоторых тождеств для функционалов,
   -- демонстрирующих трансформацию функций
   -- *************************************
   w a b = a b b

   -- ************************************************************
   -- Преобразуем используемое ниже равенство в бесточечную форму:
   --
   -- f lst == g lst = (==) (f lst) (g lst) = 
   --                = (.) (==) f lst (g lst) = 
   --                = flip ((.) (==) f) (g lst) lst =
   --                = (.) (flip ((.) (==) f)) g lst lst =
   --                = w ((.) (flip ((.) (==) f)) g) lst
   -----------------------------------------------------
   test1 lst = map (f . g) lst == (map f . map g) lst
      where f x = x+1
            g x = x-3
   --------------------------------------
   test1' = w ((.) (flip ((.) (==) f)) g)
       where f = map (f1 . g1)
             g = (map f1 . map g1)
             f1 x = x+1
             g1 x = x-3
   ---------------------------------------
   test2 lst =   (filter p . filter q) lst 
              == filter (\x -> p x && q x) lst
      where p x = x>=12
            q x = x<=26
   -------------------------------------
   test3 lst =   (filter p . concat) lst
              == (concat . map (filter p)) lst
         where p x = x>=2
   test4 = test3 [[1,2,3],[4,5,6],[7,8]]
   -----------------------------------------
   test5 lst = [f x | x <- lst] == map f lst
         where f x = x+1
   --------------------------------
   test6 lst = map id lst == id lst
   --------------------------------
   test6' :: [Integer] -> Bool
   test6' = w ((.) (flip ((.) (==) f)) g)
       where f = map id
             g = id
