   -- �㭪��, ��������� ������� ��७� �� �����-
   -- ������� �᫠ � ������� ��⮤� ���⮭�
   -----------------------------------------
   g x eps | x<0.0000000001 = 0
           | True           = mNewt x x eps
   ----------------------------------------
   mNewt a x eps | abs d < eps = x - d
                 | True        = mNewt a (x-d) eps
            where d = (x*x-a)/(2.0*x)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------------------------------
   test1 =   abs (g 16          0.0001 - 4) < 0.000000001
          && abs (g 16.000001   0.0001 - 4) < 0.000001
          && abs (g 25.000001   0.0001 - 5) < 0.000001
          && abs (g 25.00000001 0.0001 - 5) < 0.000001
   ---------------------------------------------------
   test2 = (g 1234567 0.0001, sqrt 1234567)