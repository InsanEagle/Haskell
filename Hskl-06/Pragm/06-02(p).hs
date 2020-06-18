   -- Функция, возвращающая сумму вида
   --
   --         m  m      m
   -- S(n,m)=1 +2 +...+n
   --
   ------------------------------------ 
   step:: Integer -> Integer -> Integer
   step n m = sum (map (\x -> stepQuick x m) [1..n])
   -------------------------------------------------
   step':: Integer -> Integer -> Integer
   step' n m = sum (map (`stepQuick` m) [1..n])
   --------------------------------------------------------
   -- Функция ("быстрая"), вычисляющая  m-ю степень числа n
   --------------------------------------------------------
   stepQuick:: Integer -> Integer -> Integer
   stepQuick n m | n<0 || m<0   = undefined
                 | m==0         = 1
                 | m `rem` 2==0 = stepQuick (n*n) (m `div` 2)
                 | True         = n*stepQuick (n*n)
                                              ((m-1) `div` 2)
   -- *******************************************************
   -- Неудачные тестовые примеры:
   ------------------------------
   test0 n = step n 0 == n
   test1 n = step n 1 == n*(n+1) `div` 2
   test2 n = step n 2 == n*(n+1)*(2*n+1) `div` 6               
   test3 n = step n 3 == n^2*(n+1)^2 `div` 4                   
   test4 n = step n 4 == n*(n+1)*(2*n+1)*(3*n^2+3*n-1) `div` 30
   test5 n = step n 5 == n^2*(n+1)^2*(2*n^2+2*n-1) `div` 12
   test6 n = step n 6 == (6*n^7+21*n^6+21*n^5-7*n^3+n) `div` 42
   test7 n = step n 8
   test8 n = step n 4000
