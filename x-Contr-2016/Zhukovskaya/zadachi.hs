 --- ������ ��� �������� 
 
 --- 1. ���������� ������� ������
 --- �� ���� ������� ��������� ������ ���, � ������� ������ ����� �������� 
 --- �����, � ������ �������. ������� ������ ���������� ������ ���, 
 --- � ������� ������ ����� �������� ����������, � �� ������� �������� ������
 --- ������������ ������
 
 --- 3.  ������� ������ n-�� ������� ������.

 ud :: Integer -> [Integer] -> [Integer]
 ud x lst | null lst      = []
          | x == head lst = tail lst
          | True          = (:) (head lst) (ud x (tail lst)) 
 
 test = ud 4 [1,2,3,4] == [1,2,3]
     && ud 6 []        == []
     && ud 2 [7,6,2,1] == [7,6,1]

 --- 2. ��������� ������ ������� ������ �������� ����� ���.

 povtor lst k x =concat $  map (\x -> replicate k x) lst
 
 test1 = povtor [1,2,3] 3 3 == [1,1,1,2,2,2,3,3,3]
     
 --- 4.  �������� ��������� � n-�� �� k-�� ����� [n;k).
 podsp n k lst = drop (n-1) (take (k-1) lst)
 
 test2 = podsp 2 4 [2..10] == [3,4]
 
 










       

 
 