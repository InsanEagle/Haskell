   -- ���������� ���᫥��� ���祭�� �㭪樨 y=sin(x)
   -- � ������� ࠧ������� �㭪樨 � �� ������ � ��-
   -- ��⭮�� �窨 x=0
   --
   --             3    5    7              2k+1
   --            x    x    x           k  x
   --  sin(x)=x- -- + -- - -- +...+(-1)  ------- +...
   --            3!   5!   7!            (2k+1)!
   --
   -- *********************************************
   -- ������� ���᫥��� ���祭�� �㭪樨 sin(x);
   -- k - ������⢮ ᫠������ �鸞 ������
   ----------------------------------------
   sin' x k = sum (abc x k)
   -- ****************************************
   -- �㭪��, ��������� ᯨ᮪, ᮤ�ঠ騩
   -- k ᫠������ �鸞 ������
   -----------------------------------
   abc:: Double -> Integer -> [Double]
   abc x k = [a x n | n <- [0..k]]
   -- *****************************************
   -- �㭪��, ��������� ���祭�� n-�� 童��
   -- �鸞 ������ � �窥 x
   -------------------------------
   a:: Double -> Integer -> Double
   a x k | k==0 = x
         | True = -x*x/((2*y+1)*2*y)*(a x (k-1))
       where y = fromInteger k 
   -- ******************************************
   -- �㭪��, ��������� "���⮪" �� �������
   -- ����⢥���� �ᥫ a � b 
   ----------------------------------
   perev:: Double -> Double -> Double
   perev a b = a-b*fromInt (truncate (a/b))
   -- *************************************
   -- �㭪��, ॠ������� ���஢����
   --------------------------------------------
   test :: Double -> Integer -> (Double,Double) 
   test x k = (sin' (perev x (2*pi)) k, sin x)
