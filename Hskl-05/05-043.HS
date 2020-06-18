   -- Укажите прагматику следующих функций
   -- ************************************
   import List (isPrefixOf,isSuffixOf)
   -----------------------------------
   int':: Char -> [Char] -> Bool
   int' x alph | null alph = False
               | True      = x==head alph || int' x (tail alph)
   ------------------------------------------------------------
   int1':: Char -> [Char] -> Bool
   int1' x alph | null alph    = False
                | x==head alph = True 
                | True         = int1' x (tail alph)
   -------------------------------------------------
   beg:: (->) [Char] ((->) [Char] Bool)
   beg a b | length a>length b = False
           | null a            = True
           | True              = head a==head b 
                                 && beg (tail a) (tail b)
   ------------------------------------------------------
   end:: [Char] -> [Char] -> Bool
   end a b | length a>length b = False
           | null a            = True
           | True              = last a==last b
                                 && end (init a) (init b)
   ------------------------------------------------------
   end':: [Char] -> [Char] -> Bool
   end' a b = reverse a `beg` reverse b

   -- ***************************
   -- Неудачные тестовые примеры:
   ------------------------------------------------------ 
   test1 = int' 'a' "qwertya"  && not (int' 'a' "qwerty")
   test2 = int1' 'a' "qwertya" && not (int1' 'a' "qwerty")
   test3 = int' 'a' (replicate 2000000 'b')
   ----------------------------------------
   test4 =   beg "abc" "abcd"
          && beg [] "asdasd"
          && beg [] []
          && not (beg "abc" "defg")
          && beg "abc" "abc"
   test5 =   end "abc" "qweabc"
          && end [] []
          && not (end "abc" "defg")
   --------------------------------
   test6  = isPrefixOf "abc" "abcd"
   test6' = beg        "abc" "abcd"
   test7  = isSuffixOf "abc" "asdasdaabc" 
