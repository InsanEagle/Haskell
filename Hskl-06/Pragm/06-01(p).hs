   -- Функция возвращает результат конкатенации
   -- языков в алфавите
   ------------------------------------------
   concat':: [[Char]] -> [[Char]] -> [[Char]]
   concat' lst1 lst2
            | null lst1 = []
            | null lst2 = [] 
            | True      = map (head lst1 ++) lst2
                          ++ concat' (tail lst1) lst2
   --------------------------------------------------
   concat_k:: Int -> [[Char]] -> [[Char]] -> [[Char]]
   concat_k k lst1 lst2 = map (take k) 
                              (concat' lst1 lst2)
   -- *******************************************
   -- Неудачные тестовые примеры:
   -------------------------------------------------------
   test1 = concat' ["asD","dfG","dfG","asD"] ["111","222"]
   test2 = concat' [] ["111","222"]
   test3 = concat' ["111","222"] [] 
   test4 = concat_k 3 ["asd","dfg","dfg","asd"] ["111","222"]
