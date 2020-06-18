   -- ����������:
   --  (1) ������஢���� ��������� ����������� �窨
   --      �� �몥 Haskell;
   --  (2) �ᯮ�짮����� ���������  ����������� ��-
   --      �� ��� ������஢���� ४��ᨢ��� ���᫥���
   -- ************************************************
   -- �㭪��, ����������� ��������� �����������
   -- �窨 Y �� ��।������:
   --
   --   Yf=f(Yf)
   -------------
   y f = f (y f)

   -- ****************************************************
   -- ���������� ������஢���� ४��ᨨ � �몥 Haskell,
   -- �᭮������� �� ⥮६� � ����������� �窥:
   -- ��� ��� �ଠ F �᫨ x=Fx, � x=YF
   -- ***********************************************
   -- �ਬ�� 1. ���ᬮ�ਬ ��४��ᨢ�� ��������� z
   --------------------------------------------------
   z = \x -> x+1              -- z x = x + 1
   -----------------------------------------------------
   --           �� ���������� z ����ந� �㭪樮��� z':
   -----------------------------------------------------
   z' = \f1 -> \x -> x + 1    -- z' f1 x = x + 1

   --------------------------------------------------------
   -- �ਬ�� 2. ���ᬮ�ਬ � ����⢥ �㭪樨 F ४��ᨢ���
   --           �㭪��, ��������� 䠪�ਠ� �᫠ x
   ---------------------------------------------------
   fct:: Num a => a -> a
   fct x | x==1 = 1
         | True = x * fct (x - 1)
   -----------------------------------------------------
   --           �� �㭪樨 fct ����ந� �㭪樮��� fct':
   -----------------------------------------------------
   fct':: Num a => (a -> a) -> a -> a
   fct' f1 x | x==1 = 1
             | True = x * f1 (x - 1)

   --------------------------------------------------------
   -- �ਬ�� 3. ���ᬮ�ਬ � ����⢥ �㭪樨 F ४��ᨢ���
   --           �㭪��, ��������� 䠪�ਠ� �᫠ x
   ---------------------------------------------------
   fct1 = \x -> if x==0 then 1 else x * fct1 (x - 1)
   ----------------------------------------------------------
   --           �� ���������� fct1 ����ந� �㭪樮��� fct':
   ----------------------------------------------------------
   fct1' = \f1 -> \x -> if x==0 then 1 else x * f1 (x - 1)

   ---------------------------------------------
   -- �ਬ�� 4. �஢��� ��������� �������樨
   --           � �㭪樮����� map'
   --------------------------------
   map':: (a -> b) -> [a] -> [b]
   map' f []     = []
   map' f (x:xs) = f x : map' f xs
   -----------------------------------------------------------
   --           �� �㭪樮���� map' ����ந� �㭪樮��� map'':
   -----------------------------------------------------------
   map'':: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
   map'' f1 f []     = []
   map'' f1 f (x:xs) = f x : f1 f xs

   ---------------------------------------------
   -- �ਬ�� 5. �஢��� ��������� �������樨
   --           � �㭪樮����� filter'
   -----------------------------------
   filter':: (a -> Bool) -> [a] -> [a]
   filter' p []     = []
   filter' p (x:xs) | p x  = x : filter' p xs
                    | True = filter' p xs
   -------------------------------------------------------
   --           �� �㭪樮���� filter' ����ந� �㭪樮���
   --           filter'':
   -----------------------------------------------------
   filter'':: ((a -> Bool) -> [a] -> [a]) -> (a -> Bool)
                           -> [a] -> [a]
   filter'' f1 p []     = []
   filter'' f1 p (x:xs) | p x  = x : f1 p xs
                        | True = f1 p xs

   --------------------------------------------------------
   -- �ਬ�� 6. ��������� ��᪮��筮�� 横���᪮�� ᯨ᪠
   --           � �ᯮ�짮������ ��������� Y
   -------------------------------------------
   l lst = y (\x -> lst ++ x)

   --------------------------------------------------
   -- �ਬ�� 7. �ᯮ�짮����� ��������� �����������
   --           �窨 � ᫮���� ४��ᨨ
   -------------------------------------
   fibb:: Integer -> Integer
   fibb 0     = 1
   fibb 1     = 1
   fibb (n+1) = fibb n + fibb (n-1)
   --------------------------------
   fibb' g 0     = 1
   fibb' g 1     = 1
   fibb' g (n+1) = g n + g (n-1)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ---------------------------------------------------------
   test1 = y z' 12 == 13      -- y z' 12 = z' (y z') 12 = 13
   -----------------------------------------------------------------
   test2 =   fct 10                 ==                  y fct'    10 
          && y             fct'   10==fct'             (y fct')   10   
          && fct'       (y fct')  10==fct'       (fct' (y fct'))  10
          && fct' (fct' (y fct')) 10==fct' (fct' (fct' (y fct'))) 10
   -----------------------------------------------------------------
   test3 = y fct1' 1500 == product [1..1500]
   -----------------------------------------------
   test4 = y map'' (^2) [1..5] == map' (^2) [1..5]
   test5 = y map'' sin [1..7]  == map' sin [1..7]
   ----------------------------------------------
   test6 = y filter'' (>0) [1,-1,2,-2,3,-3]
           == filter' (>0) [1,-1,2,-2,3,-3]
   test7 = (take 10 $ l [1,2,3], take 10 $ l [1])
   ----------------------------------------------
   test8 = (fibb 20,y fibb' 20)
   ----------------------------------------------------------
   -- ��������! �ਬ������ ��������� y � ����ﭭ�� �㭪樨
   -- �� �ॡ�� 㪠����� ��㬥��:
   --
   -- y (\x -> 4) = (\x -> 4) (y (\x -> 4)) = 4
   --------------------------------------------
   test9 = y (\x -> 4) 
