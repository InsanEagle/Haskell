   -- ���������� ������� ���������� ��������������
   -- ********************************************
   import Prelude hiding (enumFromTo)
   ----------------------------------
   enumFromTo:: Int -> Int -> [Int]
   enumFromTo x y | y<x  = []
                  | True = x : enumFromTo (x+1) y

   -- ***************************
   -- ��������� �������� �������:
   ----------------------------------
   test =   enumFromTo 1 4==[1,2,3,4]
         && enumFromTo 5 3==[]
         && enumFromTo 5 5==[5]
   test1 = enumFromTo 1 100