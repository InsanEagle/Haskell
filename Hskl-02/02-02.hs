   -- Демонстрация моделирования предикатов
   -- для работы с символами
   ---------------------------------------
   import Prelude hiding (isAlpha,isUpper,
                          isLower,isDigit)
   ---------------------------------------
   isAlpha:: Char -> Bool
   isAlpha c = isUpper c || isLower c
   ----------------------------------
   isUpper:: Char -> Bool
   isUpper c = c>='A' && c<='Z'
   ----------------------------
   isLower:: (->) Char Bool
   isLower c = c>='a' && c<='z'
   ----------------------------
   isDigit:: Char -> Bool
   isDigit ch = ord ch >=48 && ord ch<=57
   --------------------------------------------------------
   -- Функция, возвращающая по коду символа соответствующий 
   -- символ в "противоположном" регистре
   --------------------------------------
   abc:: Int -> Char   
   abc n | not (isAlpha w) = error "Не алфавитный символ"
         | isLower w       = toUpper w 
         | True            = toLower w
                                where w = chr n
   --------------------------------------------
   -- Неудачные тестовые примеры:
   ------------------------------------
   test1 =   isAlpha 'a' && isAlpha 'F'
          && isUpper 'B' && isLower 'a'
   ------------------------------------ 
   test2 =   abc 65=='a' && abc 90=='z' 
          && abc 97=='A' && abc 122=='Z' 
   -------------------------------------
   test3 =   isDigit '4' && isDigit '7'
          && isDigit '9'
