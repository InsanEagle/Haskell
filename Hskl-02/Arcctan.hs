   -- Демонстрация вычисления значения элементарной
   -- функции y=arcctg(x)
   -- ***********************
   arcctan:: Double -> Double
   arcctan x | x>=0 = asin (1/sqrt (1+x*x))
             | True = pi - asin (1/sqrt (1+x*x))
   ---------------------------------------------
   arcctan':: Double -> Double
   arcctan' x = pi/2 - atan x

   -- *********************************************
   -- Демонстрация вычисления значения элементарной
   -- функции y=arcsin(x)
   -- *********************
   asin' x | x==1    = pi/2
           | x==(-1) = -pi/2
           | True    = atan (x/sqrt(1-x^2))

   -- ***************************
   -- Неудачные тестовые примеры:
   ------------------------------
   test1 = arcctan 1 - pi/4
   test2 = arcctan 0 - pi/2
   test3 = 1/tan (arcctan 100.3) - 100.3
   -------------------------------------
   test4 = arcctan' 1 - pi/4
   test5 = arcctan' 0 - pi/2
   test6 = 1/tan (arcctan' 100.3) - 100.3
   --------------------------------------
   test7   = (asin 1,asin' 1)
   test8   = (asin (-1),asin' (-1))
   test9 x = (asin x,asin' x)

