   -- ������������ ���������� ����������� �����
   -- �� ����� A ������� �������
   --
   --              2      
   -- x   =x + (A/x -x )/3
   --  i+1  i      i  i   
   --                    
   -- ����� �������: f A x0,
   -- ��� A  - ����������� ���������,
   --     x0 - ��������� ����������� � �����
   -----------------------------------------
   f:: Double -> Double -> Double
   f a x | p a x = g2 a x
         | True  = f a (g2 a x)
        where g2 a x = x+(a/x/x-x)/3.0
              p a x  = abs (x-g2 a x)<0.00001

   -- ***************************
   -- ��������� �������� �������:
   ----------------------------------------------------
   test =   abs ((f  27          3.2)-3)  < 0.000000001
         && abs ((f  27.000001   3.2)-3)  < 0.000001
         && abs ((f 125.000001   5.1)-5)  < 0.000001
         && abs ((f 125.00000001 5.1)-5)  < 0.000001
         && abs ((f (6*6*6)      45.0)-6) < 0.000001
