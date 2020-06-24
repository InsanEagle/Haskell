  -- Контрольная работа №2
  -- Гончаренко Шамиль ИСИТ (2 подгруппа)
  -------------------------------------------------------


  -------------------------------------------------------

  -- Задача(1). Функция root :: Double -> Double
  --            должна вычислить приближенное значение корня уравнения cos(x) = x
  --            с точностью, заданной первым (и единственным) аргументом функции

	root :: Double -> Double
	root n = newton n (\x -> cos(x)-x) (\y -> (-sin(y)-1)) 0 

	newton epsilon f f' guess = let newGuess = guess - (f guess / f' guess)
        	                        err =  abs (newGuess - guess)
                	            in if (err < epsilon)
                        	          then newGuess
                                	  else newton epsilon f f' newGuess

  -------------- Test ---------------
	rootTest1 = root 0.1
	rootTest2 = root 0.01
  -----------------------------------             


  -------------------------------------------------------

  -- Задача(2). Функция summ :: Double -> Double -> Double
  --            вычисляет сумму ряда с общим членом. Вычисления производить
  --            пока очередной член ряда не станет по абсолютной величине меньше, чем
  --            число, заданное вторым аргументом функции.

	summ x e = f 0 1 x   
    		where f n fac p | (u <= e) = u
              		      	| otherwise = u + f (n+1) (fac*(n+1)) p*x
                 	   	  where u = p/(fac+1)

  -------------- Test ---------------
	summTest = summ 5 1000
  -----------------------------------             


  -------------------------------------------------------

  -- Задача(3). Функция ch :: Double -> Integer -> Double 
  --            должна вычислять значение гиперболического косинуса в заданной точке
  --            путем суммирования некоторого числа первых членов ряда в разложении
  --            этой функции в ряд Тейлора. Количество суммируемых членов задается вторым аргументом ф-ии.

	ch :: Double -> Integer -> Double

	fac 0 = 1
	fac k | k > 0 = k * fac (k-1)

	taylor x n = (x^2 * n')/ (2 * facn)
    		where
       		n' = fromInteger n
       		facn = fromInteger $ fac n

	ch x iter = ch' x iter 0 1
	ch' x iter n sum | iter == fromIntegral n = sum
                	 | iter /= fromIntegral n = ch' x iter (n+1) (sum + (taylor x n))                                                    
		  

  -------------- Test ---------------
	chTest1 = ch 5 10
	chTest2 = ch 10 20
	chTest3 = ch 3 3
  -----------------------------------          


  -------------------------------------------------------

  -- Задача(4). Функция harmonic :: Double -> Integer
  --            должна вычислять число членов гармонического ряда, которое нужно
  --		просуммировать, чтобы сумма ряда превысило число, заданное аргументом функции.

	harmonic :: Double -> Integer
	harmonic x = f 1 0 x 1
		where f n s x a | s > x = n-1
                                | True = f (n+1) (s+(1/a)) x (a+1)		
		                                                      
		  

  -------------- Test ---------------
	harmonicTest1 = harmonic 1.49
	harmonicTest2 = harmonic 1.5
	harmonicTest3 = harmonic 4
  -----------------------------------          
