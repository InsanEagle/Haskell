   -- ������������ ���������� ����������� �����
   -- �� ����� A ������� �������
   --
   --          A-x *x
   --             i  i        
   -- x   =x + -------.
   --  i+1  i   2*x      
   --              i
   --
   -- ����� �������: f A x0,
   -- ��� A  - ����������� ���������,
   --     x0 - ��������� ����������� � �����
   -----------------------------------------
   f:: Double -> Double -> Double
   f a x | p a x = g2 a x
         | True  = f a (g2 a x)
       where g2 a x = x+(a-x*x)/(2.0*x)
             p a x = abs (x-g2 a x)<0.00001

   -- ***************************
   -- ��������� �������� �������:
   --------------------------------------------------
   test =   abs ((f 16          4.2)-4) < 0.000000001
         && abs ((f 16.000001   4.2)-4) < 0.000001
         && abs ((f 25.000001   5.1)-5) < 0.000001
         && abs ((f 25.00000001 5.1)-5) < 0.000001