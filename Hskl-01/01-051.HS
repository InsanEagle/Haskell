   -- Демонстрация представления функции с помощью
   -- охраняющих выражений.
   --
   -- Укажите назначение приведённых функций
   -- **************************************
   ev1:: Integer -> Integer -> Integer
   ev1 x y | x-2*y==0 = error "Division by zero" 
           | True     = (x-y) `div` (x-2*y)
   ----------------------------------------
   ev3:: Int -> Int -> Double
   ev3 x y | x-2*y==0 = error "Division by zero"
           | True     = realToFrac (x-y) /
                        realToFrac (x-2*y)
   ---------------------------------------
   ev4:: Int -> Int -> Double
   ev4 x y | x-2*y==0 = error "Division by zero" 
           | True     = fromRational (toRational (x-y) /
                                      toRational (x-2*y))
   ------------------------------------------------------
   ev5:: Int -> Int -> Double
   ev5 x y | x-2*y==0 = error "Division by zero" 
           | True     = fromIntegral (x-y) /
                        fromIntegral (x-2*y)

   -- **************************
   ev2:: Float -> Float -> Float
   ev2 x y | x-2*y==0 = error "Division by zero"
           | True     = (x-y)/(x-2*y)
   ----------------------------------
   ev6:: Double -> Double -> Integer
   ev6 x y = truncate ((x-y)/(x-2*y))
   -- *******************************
   -- Неудачные тестовые примеры:
   -----------------------------------------------------
   test1 =   ev1 4 1==1 && ev2 4 1==1.5  && ev3 4 1==1.5  
          && ev1 1 0==1 && ev2 1 0==1.0  && ev3 1 0==1.0  
          && ev1 2 3==0 && ev2 2 3==0.25 && ev3 2 3==0.25 
   -------------------------------------------------------
   test2 =   ev4 4 1 ==1.5  && ev5 4 1==1.5  && ev6 4 1==1  
          && ev4 1 0 ==1.0  && ev5 1 0==1.0  && ev6 1 0==1  
          && ev4 2 3 ==0.25 && ev5 2 3==0.25 && ev6 2 3==0 
