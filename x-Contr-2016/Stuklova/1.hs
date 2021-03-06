 
   -- 21. ���������� ������� ������:

  -- �������� �������, ������� �� ���� �������� ������ ���(������ ������� ����-����� �����,
  -- ������ - ������������), � �� ������ �������� ������ ���, ��� ������ ������� ���� �������� ����������,
  -- � ������ �������� ������ ������� � ������� ��������.

  -- 22. ��������� ������ ������� ������ �������� ����� ���

  q lst n| null lst == True = []
         | True             = replicate n (head lst) ++ q (tail lst) n 

  -- �������� �������:
  test3 = q [] 2      ==  []
       && q [1,2,3] 2 == [1,1,2,2,3,3]
       && q [1,2,3] 0 == []
       && q "abc" 2   == "aabbcc'
  
  -- 26. ������� n-�� ������� ������, ������ ��� � ������
 
  del x lst i | null lst = []
              | x == i = tail lst
              | True = (:) (head lst) (del x (tail lst) (i+1))

  -- �������� �������:

  test1 = del 1 [1,2,3] 0   == [2,3]
       && del 3 [1,2,3,4] 0 == [1,2,3]
       && del 3 []  0       == []
  
  -- 24. �������� ������ � n-��� �� k-�� ����� [n,k):

  f n k = drop n (take k lst)

  -- �������� �������:
  test2 = f 1 3 [1,2,3]     == [2,3]
       && f 2 5 [1,5,7,4,3] == [7,4,3]
       && f 2 3 [5,4,3,2,1] == [3]   
       && f 1 3 "abcde"     == "bc"     