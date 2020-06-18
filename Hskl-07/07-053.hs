   -- ����������:
   --  (1) �ਬ������ ࠧ���⪨ unfoldr �� ᯨ᪠�
   --      (�� ����� List);
   --  (2) ������஢���� ࠧ���⪨ unfoldr � �������
   --      ����� ⨯�� Maybe;
   --  (3) ॠ����樨 �������� �㭪樮����� � ����-
   --      ��� ����஥���� ࠧ���⪨ unfoldr'
   --
   -- (� � � � � � � � �  ��ਠ�� ॠ����樨 unfold)
   -- **********************************************
   import List
   import Random

   -- ************************************************
   -- �ਬ������ ࠧ���⪨ unfoldr �� ������⥪� List:
   --  (1) ��� �����樨 ᯨ᪠ ���� [n,n+1,...,m];
   --  (2) ��� ����஥��� �㭪樨, �������饩 ��ப�
   --      ��⮢ ��������� �᫠ n
   --------------------------------
   enumUnfold:: Int -> Int -> [Int]
   enumUnfold n m = unfoldr (\x -> if x>m 
                                     then Nothing 
                                     else Just (x,x+1)) n
   ------------------------------------------------------
   toBinary = reverse . unfoldr step
        where step n = if n==0
                        then Nothing
                        else Just (intToDigit $ mod n 2,
                                   div n 2)

   -- ****************************************
   -- �㭪��, ����������� �㭪樮��� unfoldr
   -- �� ����� List
   ------------------------------------------
   unfoldr' :: (b -> Maybe (a,b)) -> b -> [a]
   unfoldr' f b = case (f b) of
                   Just (a,b') -> a : unfoldr' f b'
                   Nothing     -> []

   -- *********************************************
   -- ��������� �㭪樮���� zip � ������� unfoldr'
   ------------------------------------------------
   zip' :: [a] -> [b] -> [(a,b)]
   zip' = curry $ 
           unfoldr' $ \x -> case x of
                             ([],  _)    -> Nothing
                             (_,   [])   -> Nothing
                             (a:as,b:bs) -> Just ((a,b),(as,bs))

   -- *************************************************
   -- ��������� �㭪樮���� iterate � ������� unfoldr' 
   ----------------------------------------------------
   iterate' :: (a -> a) -> a -> [a]
   iterate' f = unfoldr' (\s -> Just (s,f s))

   -- ************************************************
   -- ��������� ���஢�� �롮஬ � ������� unfoldr' 
   ---------------------------------------------------
   minSort :: Ord a => [a] -> [a]
   minSort = unfoldr' (\x -> if null x 
                               then Nothing
                               else Just (minimum x, 
                                          delete (minimum x) x))

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ---------------------------------------------------------
   lstRnd = take 100 $ randomRs (1,10) (mkStdGen 7) :: [Int]
   ---------------------------------------------------------
   test1 = enumUnfold
   test2 = toBinary 1337 == "10100111001"
   test3 = take 20 (unfoldr  (\x -> Just (x,x+1)) 1)
   -------------------------------------------------
   test4 = take 20 (unfoldr' (\x -> Just (x,x+1)) 1)
   test5 = zip' [1..6] [7..12]
   test6 = take 10 (iterate' (\x -> x^2) 2)
   test7 = minSort [23,21..1] == sort [23,21..1]
   test8 = minSort lstRnd
