   -- ���������� �ᯮ�짮����� ��ࠫ���쭮� ४��ᨨ ��
   -- �ਬ�� �㭪樨, ������饩 n-�� �᫮ �������� �
   -- ������� ४��७⭮�� ᮮ⭮襭��:
   --
   --  f(0)=1, f(1)=1, f(x)=f(x-1)+f(x-2), x=2,3,... 
   -------------------------------------------------
   fibb:: Integer -> Integer
   fibb 0     = 1
   fibb 1     = 1
   fibb (n+1) = fibb n + fibb (n-1)
   --------------------------------
   fibb':: Integer -> Integer
   fibb' n | n==0 = 1
           | n==1 = 1
           | True = fibb' (n-1) + fibb' (n-2)
   ------------------------------------------
   fibb'':: Integer -> Integer
   fibb'' n 
      | n==0 = 1                        
      | n==1 = 1                        
      | True = if even n
                 then fibb'' t * fibb'' (t-1) +
                      2 * fibb'' (t-1) * fibb'' (t-1) + (-1)^t
                 else 2 * fibb'' s * fibb'' (s+1) -
                      fibb'' s * fibb'' s
      where t = n `div` 2
            s = (n-1) `div` 2

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ----------------------------------------
   test1 = fibb  11==144 && fibb  23==46368
   test2 = fibb' 11==144 && fibb' 23==46368
   test3 = fibb  30==fibb' 30
   test4 = fibb'' 11==144 && fibb'' 23==46368
   test5 = fibb'' 11==144 && fibb'' 23==46368
   test6 = fibb'' 130 + fibb'' 131==fibb'' 132
