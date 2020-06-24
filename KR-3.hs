  -- Контрольная работа №3
  -- Гончаренко Шамиль ИСИТ (2 подгруппа)
  -------------------------------------------------------


  -------------------------------------------------------

  -- Задача(1). Функция maxthree :: [Integer] -> [Integer]
  --            получает список целых и выдает список той же длины, содержащий в 
  --            качестве i-го элемента максимальные значения трех элементов списка
  --            i-го, (i-1)-го и (i+1)-го.

	maxthree :: [Integer] -> [Integer]
	maxthree lst = init (a ((-1):((-1):lst)))
		where a [] = []
                      a [x] = [] 
		      a (x:xs) = ((maximum(take 3 xs)):[]) ++ a xs 
                       


  -------------- Test ---------------
	maxthreeTest1 = maxthree [3,8,6,5,1]
        maxthreeTest2 = maxthree [10,4,6,8,1,12]
  -----------------------------------             


  -------------------------------------------------------

  -- Задача(2). Функция summator :: [Integer] -> [Integer]
  --            получает список целых и выдает список, содержащий
  --            в качестве i-го элемента суммы первых i членов исходного списка

	summator :: [Integer] -> [Integer]
	summator lst = a lst [] 1
		where a lst lst2 n | n <= length lst = a lst (sum(take n lst):lst2) (n+1)
                                   | True = reverse lst2  

  -------------- Test ---------------
	summatorTest1 = summator [3,2,6,5,1]
	summatorTest2 = summator [1..100]		
  -----------------------------------             


  -------------------------------------------------------

  -- Задача(3). Функция maxString :: [String] -> String 
  --            выдает самую длинную строку заданного списка.

	maxString :: [String] -> String
	maxString str = foldl1 (\acc x -> if length acc > length x then acc else x) str	


  --------------- Test --------------
	maxStringTest1 = maxString ["Find","the","longest","word","in","this","list"]          
  -----------------------------------

  -------------------------------------------------------

  -- Задача(4). Функция removeEven :: [String]-> [String]
  --            по заданному списку строк строит список, в котором содержатся те же
  --            строки, что и в исходном списке, но каждая вторая строка из списка
  --            выброшена, а в оставшихся строках выброшен каждый второй символ

	removeEven' :: [a] -> [a]
	removeEven' (x:_:xs) = x: removeEven' xs
	removeEven' xs       = xs
 
	removeEven :: [[a]] -> [[a]]
	removeEven = map removeEven' . removeEven'

		                                                      
		  

  -------------- Test ---------------
	removeEvenTest1 = removeEven ["one", "two", "three", "four", "five"]
  -----------------------------------          
