  -- ����������� ������ �3
  -- ���������� ������ ���� (2 ���������)
  -------------------------------------------------------


  -------------------------------------------------------

  -- ������(1). ������� maxthree :: [Integer] -> [Integer]
  --            �������� ������ ����� � ������ ������ ��� �� �����, ���������� � 
  --            �������� i-�� �������� ������������ �������� ���� ��������� ������
  --            i-��, (i-1)-�� � (i+1)-��.

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

  -- ������(2). ������� summator :: [Integer] -> [Integer]
  --            �������� ������ ����� � ������ ������, ����������
  --            � �������� i-�� �������� ����� ������ i ������ ��������� ������

	summator :: [Integer] -> [Integer]
	summator lst = a lst [] 1
		where a lst lst2 n | n <= length lst = a lst (sum(take n lst):lst2) (n+1)
                                   | True = reverse lst2  

  -------------- Test ---------------
	summatorTest1 = summator [3,2,6,5,1]
	summatorTest2 = summator [1..100]		
  -----------------------------------             


  -------------------------------------------------------

  -- ������(3). ������� maxString :: [String] -> String 
  --            ������ ����� ������� ������ ��������� ������.

	maxString :: [String] -> String
	maxString str = foldl1 (\acc x -> if length acc > length x then acc else x) str	


  --------------- Test --------------
	maxStringTest1 = maxString ["Find","the","longest","word","in","this","list"]          
  -----------------------------------

  -------------------------------------------------------

  -- ������(4). ������� removeEven :: [String]-> [String]
  --            �� ��������� ������ ����� ������ ������, � ������� ���������� �� ��
  --            ������, ��� � � �������� ������, �� ������ ������ ������ �� ������
  --            ���������, � � ���������� ������� �������� ������ ������ ������

	removeEven' :: [a] -> [a]
	removeEven' (x:_:xs) = x: removeEven' xs
	removeEven' xs       = xs
 
	removeEven :: [[a]] -> [[a]]
	removeEven = map removeEven' . removeEven'

		                                                      
		  

  -------------- Test ---------------
	removeEvenTest1 = removeEven ["one", "two", "three", "four", "five"]
  -----------------------------------          
