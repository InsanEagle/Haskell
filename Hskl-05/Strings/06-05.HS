   -- Демонстрация нахождения суммы номеров заданной
   -- буквы в заданном слове
   ----------------------------------------------
   sumN x w = summa x (pairlis w (take (length w) 
                                       (enumFrom 1)) [])
   -----------------------------------------------------
   summa:: Char -> [(Char,Integer)] -> Integer
   summa x lst | null lst          = 0
               | fst (head lst)==x = snd (head lst)+
                                     summa x (tail lst)
               | True              = summa x (tail lst)
   ----------------------------------------------------
   -- Функция конструирует список пар из списка lkey и
   -- списка ldata путем добавления новых пар в начало
   -- существующего списка alist
   -------------------------------------------------
   pairlis:: String -> [Integer] -> [(Char,Integer)]
                                 -> [(Char,Integer)]
   pairlis lkey ldata alist | null lkey||null ldata
                                   = alist
                            | True = (head lkey,head ldata):
                                     pairlis (tail lkey)
                                             (tail ldata)
                                             alist
   -----------------------------------------------
   test =   sumN 'A' "aaaaa"      == 0
         && sumN 'O' "O"          == 1
         && sumN 'о' "олово"      == 1+3+5
         && sumN 'a' "aaaaa"      == 1+2+3+4+5
         && sumN '1' "1212121212" == 1+3+5+7+9

