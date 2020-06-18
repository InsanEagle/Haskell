   -- ���������� ������ ���᫥��� � �몥 Haskell
   -- *************************************************
   -- �᭮���� �㭪�� ���� ��६�����
   --------------------------------------------
   d:: Integer -> Integer -> Integer -> Integer
   d x y z = x+y*z
   -- ********************************
   -- ������ �㭪樨 ��� �㭪樨 d
   ---------------------------------------------
   d1:: Integer -> Integer -> Integer -> Integer
   d1 x y = \z -> x+y*z 
   ---------------------------------------------
   d2:: Integer -> Integer -> Integer -> Integer
   d2 x = \y z -> x+y*z 
   ---------------------------------------------
   d3:: Integer -> Integer -> Integer -> Integer
   d3 = \x y z -> x+y*z 
   ----------------------------------------------
   d3':: Integer -> Integer -> Integer -> Integer
   d3' = flip ((.) (flip ((.) (.) (+))) (*))
   -- **************************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test =   (d    2  3  4) == 14
         && (d1   2  3) 4  == 14
         && (d2   2) 3  4  == 14
         && (d3)  2  3  4  == 14
         && (d3') 2  3  4  == 14
