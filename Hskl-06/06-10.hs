   -- Функция, семантически эквивалентная функционалу
   -- partition из библиотеки List;
   --  res - накапливающий параметр (при запуске res=([],[]))
   ----------------------------------------------------------
   partition' pred lst (res1,res2)
         | null lst        = (res1,res2)
         | pred $ head lst = partition' pred (tail lst)
                                             (res1 ++ [head lst],res2)
         | True            = partition' pred (tail lst)
                                             (res1,res2 ++ [head lst])

   -- ****************************************
   partition:: (a -> Bool) -> [a] -> ([a],[a])
   partition p xs = foldr select ([],[]) xs
        where select x (ts,fs) | p x  = (x:ts,fs)
	                       | True = (ts,x:fs)

   -- ***************************
   -- Неудачные тестовые примеры:
   --------------------------------------------
   test1 = partition  (>0) [1,-2,3,4,5,-3,2,-5]
   test2 =    partition' (>0)  [1,-2,3,4,5,-3,2,-5] ([],[])
           == partition  (>0)  [1,-2,3,4,5,-3,2,-5]
   test3 = partition' (<0)  [1,2,3,4,5,2] ([],[])
   test4 = partition' (==0) [1,2,3,4,5,-3,2,-5] ([],[])
