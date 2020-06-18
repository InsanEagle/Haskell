   -- Укажите прагматику приведённых ниже функций
   ----------------------------------------------
   cdr lst | null lst = lst
           | True     = tail lst
   -----------------------------
   doub lst | null lst = lst
            | True     = head lst : lst
   ---------------------------------------------------
   posl lst | null lst = error "Недопустимый аргумент" 
            | True     = (head.reverse) lst
   ----------------------------------------
   inlist:: Eq a => [a] -> Bool
   inlist lst = (.) (flip ((.) elem head)) tail lst lst 
   ----------------------------------------------------
   swap:: (->) [a] [a]
   swap lst = reverse 
                 (head lst: 
                    reverse (head (reverse lst):
                             reverse (tail (reverse (tail lst))))
                 )
   ----------------------
   predct:: [Int] -> Bool
   predct lst | null lst                      = False
              | sum lst `mod` (length lst)==0 = True
              | True                          = False
   --------------------------------------------------
   prodsum:: [Int] -> Int
   prodsum lst = product lst - sum lst
   -----------------------------------
   sqList:: (->) [Int] Bool
   sqList lst | null lst                       = undefined
              | product lst==fact (length lst) = True
              | True                           = False
     where fact n | n==0 = 1
                  | True = n*fact (n-1)
   ------------------------------------  
   abc:: (->) [Int] ((->) [Int] [Int])
   abc lst1 lst2 | sum lst1==sum lst2 && lst1==lst2 = lst1
                 | True                             = lst2
   -------------------------------------------------------
   -- Неудачные тестовые примеры:
   ------------------------------
   test1 =   predct [1,2,3]
          && predct [1,2,3,4,5] 
          && not (predct [1,7,3,4])
          && not (predct []) 
   ------------------------------------
   test2 =   prodsum [1,2,3]     ==   0
          && prodsum [1,2,3,4,5] == 105 
   -----------------------------------------------
   test3 =   swap [1,2,5,8]           == [8,2,5,1]
          && swap [1,7,8,9,89]        == [89,7,8,9,1]
          && swap [1,89,0,8,7,9,80]   == [80,89,0,8,7,9,1]
          && swap [9,8,7,6,5,4,2,3,1] == [1,8,7,6,5,4,2,3,9]
          && swap [1,5,6]             == [6,5,1]
          && swap [1,5]               == [5,1]
          && swap [9,0,8,6,7,5]       == [5,0,8,6,7,9]
          && swap [1,2,3,4,5,6,7,8,9] == [9,2,3,4,5,6,7,8,1]
   ---------------------------------------------------------
   test4 =   not (sqList [1,2,3,4,6,7,8])
          && not (sqList [3,4,6,7,8])
          && not (sqList [4,6,7,8])
          && not (sqList [11,6,7,8])
          && sqList [1,2,3,4]
   ----------------------------------------------
   test5 =   abc [9,8,5,7] [4,6,6,7] == [4,6,6,7]
          && abc [1,2,3] [7,8,9]     == [7,8,9]
          && abc [2,3,1] [2,3,1]     == [2,3,1]
   --------------------------------------------
   test6 =   not (inlist [1,2,3,4])
          && inlist [1,2,1,4]
          && inlist [1,1,1,1]