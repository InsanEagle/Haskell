   -- ��㤠�� ॠ����樨 �㭪権 � �⨫�� foldr- � foldl-
   -- *****************************************************
   import List

   -- ***********************
   unfold p f g x | p x  = [] 
                  | True = f x : unfold p f g (g x)

   -- ************************
   -- ��������� ����樨 elem
   ----------------------------------------------------
   elem' x = foldr (||) False . foldr ((:) . (== x)) []

                                               -- foldr-foldr-�⨫�
   -- ***************************
   -- ��������� �㭪樨 reverse.
   -- ����: �.�.��ࠪ��� (09.12.2015)                        
   -----------------------------------
   reverse' = foldr f e
        where f = \x y -> foldr (:) [x] y
              e = []                                 -- foldr-�⨫�

   -- ********************************************
   -- ��������� �㭪樨 tail ��� ����筮�� ᯨ᪠
   -- (����䥪⨢��� ॠ������: ��� ��室�).
   --
   -- ����: �.�.����楢� (21.10.2015)
   -------------------------------------
   tail'''' lst = foldr f e lst                       
        where f = \x xs -> if length xs == n
                             then xs else x : xs
              e = []
              n = -1 + length lst                    -- foldr-�⨫�

   -- ***********************
   -- ��������� �㭪樨 init
   -- (����䥪⨢��� ॠ������: ��� ��室�)
   --
   -- ����: �.�.����楢� (21.10.2015)
   -------------------------------------
   init'' lst = foldl f e lst                        
        where f = \x xs -> if length x == n
                             then x else x ++ [xs]
              e = []
              n = -1 + length lst                    -- foldl-�⨫�

   -- ***********************
   -- ��������� �㭪樨 last
   -------------------------------
   last' = foldr const undefined . 
           foldr (\x xs -> xs ++ [x]) []       -- foldr-foldr-�⨫�

   -- ************************************
   -- �㭪�� ॠ����� �㭪樮��� filter
   -- (����䥪⨢��� ॠ������: �ॡ���� ����� ��室��).
   --
   -- ����: �.�.���檨� (����� 2015)
   --------------------------------------------------
   filter' p x = (foldr (.) id . unfold null f g) x x
        where f x = if (p . head) x               
                      then id               
                      else delete (head x)
              g x = tail x                    -- foldr-unfold-�⨫�

   -- ******************************************
   -- �㭪��, ����������� �㭪樮��� dropWhile
   -- (�� ࠡ�⠥� � ��᪮���묨 ᯨ᪠��).
   --
   -- ����: �.�.����楢� (21.10.2015)
   -------------------------------------------
   dropWhile' p lst = drop (foldr f e lst) lst
        where f = \x n -> if p x then n + 1 else 0
              e = 0                       
                                          -- ���ᯮ�������� �⨫�

   -- ****************************************
   -- ��������� �㭪樮���� scanr, �᭮������
   -- �� ᫥���饬 ��।������ scanr:
   --
   --   scanr f e = map (foldr f e) . tails 
   --
   -- [Bird,1998,p.125]
   ---------------------------------------------
   scanr' f e lst = (foldr ((:) . foldr f e) [])
                    (unfold null id tail lst ++ [[]])

                                          -- ���ᯮ�������� �⨫�

   -- ****************************************
   -- ��������� �㭪樮���� scanl, �᭮������
   -- �� ᫥���饬 ��।������:
   --
   --  scanl f e = map (foldl f e) . inits
   --
   --  [Bird,1998,p.124]
   ---------------------------------------
   scanl' f e = foldr ((:) . foldl f e) [] 
                           . ((++) [[]]) . reverse
                           . unfold null id init

                                          -- ���ᯮ�������� �⨫�

   -- *******************************************************
   -- �㭪�� intercalate �ਭ����� ᯨ᮪ x � ᯨ᮪ ᯨ᪮�
   -- xs � ࠧ��頥� ᯨ᮪ x ����� ᯨ᪠�� � xs
   ----------------------------------------------
   intercalate':: [a] -> [[a]] -> [a]
   intercalate' x = concat . foldr f e
        where f x' xs = x' : if null xs
                               then xs else x : xs
              e       = []
                                             -- "����" foldr-�⨫�

   -- *************************
   -- ��������� �㭪樨 unzip3
   ---------------------------------------------------------
   unzip3' = foldl (\(a,b,c) (d,e,f) -> (a ++ [d], b ++ [e],
                                         c ++ [f]))
                   ([],[],[])
                                                     -- foldl-�⨫�

   -- *************************
   -- ��������� �㭪樨 lines.
   -- ����: http://smokycat.info/haskell/604 (26.03.2013)
   -------------------------------------------------------
   lines' str = foldr f e str str id
        where f _ next ('\n':ys) tmp = tmp []  : next ys id
              f _ next (y:[])    tmp = tmp [y] : next [] id
              f _ next (y:ys)    tmp = next ys (tmp . (y :))
              e _ _                  = []
                                                     -- foldr-�⨫�

   -- ***************************
   -- ��������� �㭪樨 unlines.
   -- ����: http://smokycat.info/haskell/604 (26.03.2013)
   -------------------------------------------------------
   unlines' = foldr f e
        where f x next = x ++ ('\n' : next)       
              e        = ""                          -- foldr-�⨫�

   -- ***************************
   -- ��������� �㭪樨 unwords.
   -- ����: http://smokycat.info/haskell/604 (26.03.2013)
   -------------------------------------------------------
   unwords' = foldr f e
        where f x next = x ++ (' ' : next)        
              e        = ""                          -- foldr-�⨫�

   -- *********************************************
   -- ��������� �㭪樨 tail ��� ����筮�� ᯨ᪠.
   -- ����: �.�������� (1 ����, ��, 10.11.2015)
   ---------------------------------------------
   tail' = foldr f [] . h1
        where f (x,i) ys = if i==0           -- "����" foldr-�⨫�
                             then ys
                             else x : ys
              --------------------------
              h1 :: [a] -> [(a,Int)]
              h1 []     = []
              h1 (x:xs) = (x,0) : map (\y -> (y,1)) xs

   -- ************************
   -- ��������� �㭪樨 init.
   -- ����: �.�������� (1 ����, ��, 10.11.2015)
   ---------------------------------------------
   init' = foldr f [] . h2
        where f (x,i) ys = if i==0 
                             then ys 
                             else x : ys
              --------------------------
              h2 :: [a] -> [(a,Int)]
              h2 []     = []
              h2 [x]    = [(x,0)]
              h2 (x:xs) = (x,1) : h2 xs      -- "����" foldr-�⨫�

   -- ******************************************
   -- �㭪��, ����������� �㭪樮��� dropWhile
   -- (ࠡ�⠥� � ��᪮���묨 ᯨ᪠��).
   --
   -- ����: �.�������� (1 ����, ��, 10.11.2015)
   ---------------------------------------------
   dropWhile''' p = foldr f' [] . (g p)
        where f' (x,b) ys = if b             -- "����" foldr-�⨫�
                              then ys
                              else x : ys
              -------------------------------------
              g :: (a -> Bool) -> [a] -> [(a,Bool)]
              g p [] = []
              g p (x:xs) = if p x
                             then (x,True) : g p xs
                             else (x,False) :
                                  map (\x -> (x,False)) xs

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ----------------------------------------------
   test1 = dropWhile' (even) [1,5,3,-2,3,4,-2] == 
                             [1,5,3,-2,3,4,-2]
   test2 = dropWhile' (>0) [1,5,3,-2,3,4,-2]   == [-2,3,4,-2]
   test3 = dropWhile' (even) [2,-4,3,-2,3,4,-2] 
                           == [3,-2,3,4,-2]
   test4 = dropWhile' (<0) [1,5,3,-2,3,4,-2]  
                           == [1,5,3,-2,3,4,-2]

   test5 =   dropWhile''' (<5) [1..10]                   == [5..10]
          && dropWhile''' (not . null) [[1],[52],[],[4]] == [[],[4]]
          && dropWhile''' (>2) []                        == []
   test6 = dropWhile''' (==0) [0,1,2,3,-3,-3] ==
           dropWhile    (==0) [0,1,2,3,-3,-3]   
   -------------------------------------------------
   test7 = scanl' (+) 0 [1..5] == scanl (+) 0 [1..5]
   test8 = scanr' (+) 0 [1..5] == scanr (+) 0 [1..5]
   -------------------------------------------------
   test9  = intercalate' [1,2,3] [[1,1],[2,2],[3,3]]
   test10 = intercalate' " " ["Avoid","success","at","all","costs"]
            == "Avoid success at all costs"
   test11 = unzip3' [(1,2,3),(11,22,33)]
