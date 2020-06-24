  -- ����������� ������ �1
  -- ���������� ������ ���� (2 ���������)
  -------------------------------------------------------


  -------------------------------------------------------

  -- ������(1). ������� amember :: Double -> Double -> Int -> Double
  --            ������ ���������� n-�� ���� �������������� ���������� 
  --            (����� ���������� ���������� � ����). ����������� ������� 
  --            �������� ������ ���� ����������, ���, n.
  --            ��������,
  --            > amember 1 2 5
  --            11

	amember :: Double -> Double -> Int -> Double
	amember x y z = y * fromIntegral z + x 

  -------------- Test ---------------
	amemberTest1 = amember 1 2 5
	amemberTest2 = amember 1.3 2.2 10
	amemberTest3 = amember (-3.2) (-1.1) 4
  -----------------------------------             


  -------------------------------------------------------

  -- ������(2). ������� mutual :: Integer -> Integer -> Boolean
  --            ����������, �������� �� �������� ��� ����������� ����� ������� ��������.
  --            ��������,
  --            > mutual 25 14    > mutual 12 39
  --            True,               False

	mutual :: Integer -> Integer -> Bool
	mutual x y | gcd x y == 1 = True
                   | True = False  

  -------------- Test ---------------
	mutualTest1 = mutual 25 14
	mutualTest2 = mutual 12 39
	mutualTest3 = mutual 30 100
  -----------------------------------             


  -------------------------------------------------------

  -- ������(3). ������� sumdiv :: Integer -> Integer 
  --            ������ ����� ���� ��������� ��������� �������������� �����, 
  --            ������� �������, �� �������� ���� ��� �����.
  --            ��������,
  --            > sumdiv 36
  --            1+2+3+4+6+9+12+18 = 55

        sumdiv :: Integer -> Integer
	sumdiv x = prov a x 0
		where prov a 1 s = s
		      prov a x s | a `mod` x == 0 = prov a (x-1) s+a `div ` x
                                 | True = prov a (x-1) s
                      a = x  
                                                            
		  

  -------------- Test ---------------
	sumdivTest1 = sumdiv 36
	sumdivTest2 = sumdiv 100
	sumdivTest3 = sumdiv 2
  -----------------------------------          


  -------------------------------------------------------

  -- ������(4). ������� inverse :: Integer -> Integer
  --            ������ �� ��������� ����� �������� �����, ���������� ������ 
  --            �������� �������� ����� ��������� ����� � �������� �������.
  --            ��������,
  --            > inverse 36     > inverse 1050
  --            63               501

        inverse :: Integer -> Integer
	inverse x = read(reverse(show x))                                                     
		  

  -------------- Test ---------------
	inverseTest1 = inverse 36
	inverseTest2 = inverse 1050
  -----------------------------------          
