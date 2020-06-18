  -- ������������ ������ �3
  -- ������� �������� �� �������� ����������.
  -- ������� 9(1)
  -- ���������� ������ ���� (2 ���������)
  -------------------------------------------------------


  --------------------- ������ 1 ------------------------


  -- �������(1). ������� ���������� (����������) ����������� �������.

  -- 1) ���� �������� m = 0, ����� ������� 0, � ������ ������
  --    ������������ � �������, �������� ����� �������� m ������� ������ ������ m - 1 � ���������� � ���������� n.
  --    �.�. ������� ��������� m * n.

  -- 2) ���� m = 0, ����� ������� 0, � ������ ������ �������� �������������� ��� ������ �������� ���������.
  --    ��� ������ ���� ����������� ������� �������������� ���������� ����� n � ������� m.

  -- 3) ������� ��������� ���������� ������� n �� p, ���� n>p.

  -- 4) ������� ������� ������� ������������� �����.

  -------------------------------------------------------

  -- �������(2). �������� ��������� ��� ���������� �������� ����������� ����
  --             ������� � ��������� � �� ������� F(5,0,0), F(1000,0,0). 

    f1 x y z | x == 0 = 0
             | (y+1) == x = z
             | True = f1 x (y+1) (z+1)

  -- Test --
    f1Test1 = f1 5 0 0 
    f1Test2 = f1 1000 0 0
  -----------------------------------

  --------------------- ������ 2 ------------------------

  -- �������(4(1)). �������� �������, ����������� �������� �������.

    f2 n | n == 0 = 1
         | n == 1 = 4
         | True = f2 (n-1)*4

  -- Test --
    f2Test = f2 5  

 ----------------------------------------

  -- �������(6(1)). �������� ���������, ����������� �������� ��������� �������. 

    f3 n | n == 1 = 1
         | True = f3 (n-1)+n

  -- Test --
    f3Test = f3 10

  -----------------------------------

  -- �������(7). �������� �������, ����������� �������� ������� (n!)!. 

    f4 0 = 1
    f4 n | n > 0 = n * f4 (n-1)        
         
  --Test--
    f4Test = f4 8
  --------------------- ������ 3 ------------------------

  -- �������(1). �������� ������� �� ����� Haskell, �������� ���: 

    f5 n | (n>202) = (n-3)
         | True = f5 (f5 (n+4))

  --Test--
    f5Test = f5 300 
  -------------------------------

  -- �������(5.1). �������� �������, ����������� �������� �� ��������� ������������ �����������

    f6 n | n == 0 = 1
         | n == 1 = -6
         | True = 6*(f6 (n-1)) - 9*(f6 (n-2))

  --Test--
    f6Test = f6 15

  --------------------- ������ 4 ------------------------


  -- �������(1)

    f7:: Float -> Float
    f7 x = asin x

    f8 :: Float -> Float 
    f8 x = acos x

    f9 :: Float -> Float
    f9 x = atan x

    f10 :: Float -> Float
    f10 x = (pi/2) - atan x

    f11 :: Float -> Float
    f11 x = acos (1/x) -- arcsec x
    
    f12 :: Float -> Float
    f12 x = asin (1/x) -- arccossec x


  --Test--
    f7Test  = f7 30
    f8Test  = f8 60
    f9Test  = f9 45
    f10Test = f10 15
    f11Test = f11 45
    f12Test = f12 68 











 