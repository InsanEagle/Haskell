   -- ���������� ���⥫쭮�� �९�����⥫�� �� ������
   -- ��।������ "������" ����ࠫ쭮�� �᫠
   -- (05.09.2015)
   -- ****************************************************
   -- ���� ᯮᮡ. �।����, ��।����騩 "������" ��-
   --                ��ࠫ쭮�� �᫠ p; �� ����᪥ d=2
   -----------------------------------------------------
   prime1:: Integer -> Integer -> Bool
   prime1 p d | d*d > p      = True
              | p `mod` d==0 = False
              | True         = prime1 p (d+1)

   -- *********************************************
   -- ��ன ᯮᮡ. �㭪��-�।����, �ᯮ������
   --                ���⮥ �᫮ x
   ---------------------------------
   prime2 n | n==2         = True
            | n `mod` 2==0 = False
            | True         = prime2' n 3 (truncate $ sqrt (fromInteger n))
   -----------------------------------------------------------------------
   prime2' n k m | k>m  = True
                 | True = n `mod` k/=0 && prime2' n (k+2) m
   --------------------------------------------------------
   test2 = map (\x -> (x,prime2 x)) [2..20]

   -- *********************************************
   -- ��⨩ ᯮᮡ. �㭪��-�।����, �ᯮ������
   --                ���⮥ �᫮ x
   ---------------------------------
   prime3 n | n==2         = True
            | n `mod` 2==0 = False
            | True         = prime3' n 3 (truncate $ sqrt (fromInteger n))
   -----------------------------------------------------------------------
   prime3' n k m | k>m          = True
                 | n `mod` k/=0 = prime3' n (k+2) m
                 | True         = False
   ----------------------------------------
   test3 = map (\x -> (x,prime3 x)) [2..20]

   -- ************************************************
   -- ������� ᯮᮡ. �㭪��-�।����, �ᯮ������
   --                   ���⮥ �᫮ x
   ------------------------------------
   isPrime x = dividers x==[1,x]
   ------------------------------------------
   dividers n = [x | x <- [1..n], mod n x==0]

   -- ********************************************
   -- ���� ᯮᮡ. �㭪��-�।����, �ᯮ������
   --                   ���⮥ �᫮ x
   ------------------------------------
   prime5:: Integer -> Bool
   prime5 n = all (\x -> n `mod` x /=0) [2..m]
       where m = truncate $ sqrt (fromInteger n)
   ---------------------------------------------
   test5 = map (\x -> (x,prime5 x)) [2..20]

   -- ************************************************
   -- �㭪�� �����頥� �� ����� �᫠ �� [2..500]
   ---------------------------------------------------
   f1 = map (\x -> (x,prime1 x 2)) [2..500]

   -- *********************************************************
   -- �ࠣ��⨪� ᫥����� �㭪権 ��⠭���� ᠬ����⥫쭮...
   ---------------------------------------------------------------
   f2 n = toRational (length $ filter (\x -> prime1 x 2) [2..n]) /
          (toRational n)  
   f3 = map (\x -> fromRational $ f2 x) [100,200..50000]

   -- **************************************************
   -- ��襭�� ����� � ��⠢����� ᯨ᪠ ������ �ᥫ,
   -- ������ ��� 2^n+1 (��� �� 㤠���� ������� �����
   -- 5 ������⮢!)
   -----------------------------------------------
   abc = filter prime3 $ map (\n -> 2^n + 1) [1..]