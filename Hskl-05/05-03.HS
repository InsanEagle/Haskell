   -- Функция, возвращающая количество символов
   -- в списке lst, равных заданному символу s
   -------------------------------------------
   kolEl:: Char -> [Char] -> Int
   kolEl s lst | null lst || notElem s lst
                             = 0
               | head lst==s = 1 + kolEl s (tail lst)
               | True        = kolEl s $ tail lst

   -- ***************************
   kolEl':: Char -> [Char] -> Int
   kolEl' s lst | null lst    = 0
                | last lst==s = 1 + kolEl' s (init lst)
                | True        = kolEl' s $ init lst

   -- ***************************
   -- Неудачные тестовые примеры:
   ------------------------------------------
   test1 =   kolEl 'n' ['a','n']         == 1
          && kolEl 'a' ['f','a','b','a'] == 2 
          && kolEl 'v' ['a','s','d']     == 0 
          && kolEl 'q' "qwqwqwqweb"      == 4 
          && kolEl ' ' "q wq qw we "     == 4
   -------------------------------------------
   test2 =   kolEl' 'n' ['a','n']         == 1
          && kolEl' 'a' ['f','a','b','a'] == 2 
          && kolEl' 'v' ['a','s','d']     == 0 
          && kolEl' 'q' "qwqwqwqweb"      == 4 
          && kolEl' ' ' "q wq qw we "     == 4
