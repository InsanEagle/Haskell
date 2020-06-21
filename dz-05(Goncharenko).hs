  -- ������������ ������ �5
  -- ������� �������� �� �������
  -- ������� 9(1)
  -- ���������� ������ ���� (2 ���������)
  -------------------------------------------------------


  --------------------- ������ 1 ------------------------


  -- �������(4). �������� �������, ������������ ���������� �� ������������
  --             �������� ����������� ��������� ��������� ������.


	f1 lst = maximum (a lst)
		where a [x] = [x]
		      a (x:xs)   = (b x xs) ++ (a xs)
		      b x []     = []
		      b x (y:ys) = (x*y) : b x ys
     

  -------------- Test ---------------
	f1Test = f1 [1,2,5,3,10,4]
  -----------------------------------
               

  --------------------- ������ 2 ------------------------


  -- �������(1). �������� �������, ������������ ������� �� ������ ��� ������������ � ������.


	f2 1 (x:xs) = x
	f2 n (x:xs) = f2 (n-1) (xs)  
		
     

  -------------- Test ---------------
	f2Test = f2 3 [1,2,5,3,10,4]
  -----------------------------------

  -------------------------------------------------------

  -- �������(4). �������� ���������, ������������� ����� (�� ����� 100 ������)
  --             � ���������� ����� "�������", ���� ����� �������, � "���" � ��������� ������.


	f3 n | n `div` 10 == 0 = "Plavnoe"
             | True = (f3'.lst') n
		where lst' n = lst'' n []
			where lst'' 0 lst = lst
                              lst'' n lst | length lst > 100 = undefined
                                        | True = lst'' (n`div`10) ((n`mod` 10):lst)
                      f3' [x] = "Plavnoe"
                      f3' (x:xs) | abs(x-head xs) /= 1 = "Ne plavnoe"
                                 | True = f3' xs
	  


		
     

  -------------- Test ---------------
	f3Test1 = f3 12345
	f3Test2 = f3 134510
	f3Test3 = f3 654323456
  -----------------------------------


  --------------------- ������ 3 ------------------------



  -- �������(3). �������� �������, �������������� ������������ ������� �������� ��������� ������.

	f4 [] = []
	f4 [x] = [x]
	f4 (x:y:xs) = y:x:f4 xs


  -------------- Test ---------------
	f4Test = f4 [1..10] 
  -----------------------------------
 

  -------------------------------------------------------

  -- �������(4). �������� �������, ������� �� ���� �������� ������� �������
  --             ������� ������, �������� �������� ���������� ����� ��������
  --             ��������� �������� �������, ������� �� ���������� ��������.

	f5 [] [] = []
	f5 [] [y] = y:[]
	f5 [x] [] = x:[]
	f5 (x:xs) [] = x:[] ++ f5 (xs) []
	f5 [] (y:ys) = y:[] ++ f5 [] (ys)
	f5 [x] [y] = x+y:[] 
	f5 (x:xs) (y:ys) = x+y:[] ++ f5 (xs) (ys)


  -------------- Test ---------------
	f5Test1 = f5 [1..10] [1..10]
	f5Test2 = f5 [1..10] [1..5] 
	f5Test3 = f5 [2,4..10] [1..10]
  -----------------------------------

  --------------------------------------------------------

  -- �������(6). �������� �������, ���������� ������ ������� ��������� ���������
  --             (�.� ������), � ����� ���� ��������� ���������.

                                                 
	f6 [] = []
	f6 [x] = x:[] 
        f6 (x:xs) = f6 xs ++ x:[] 


  -------------- Test ---------------
	f6Test1 = f6 [1..10]
  -----------------------------------

  -------------------------------------------------

  -- �������(7). �������� �������, ��������� �� ������ ��������� ��������� ���������.


	f7 [] = []
	f7 [x] = [x]
        f7 (x:xs) | elem x xs = x:f7 (del x xs)
	          | True = x:f7 xs
		where   del n [] = [] 
			del n lst | n == head lst = del n (tail lst)
                                  | True = head lst:del n (tail lst)     


  -------------- Test ---------------
	f7Test1 = f7 [1,5,2,3,4,5,5,5,5,6,7,10]
	f7Test2 = f7 [5,123,1,2,3,3,3,1]
	f7Test3 = f7 [1,2,1,2,1,2,3,1,2,1,2,3,1,2,3]
  -----------------------------------




