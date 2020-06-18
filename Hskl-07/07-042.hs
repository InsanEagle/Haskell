   -- ���������� �ਣ���-�ணࠬ��஢���� �
   --   fix-foldr-,  fix-foldl-,
   --   fix-unfold-, fix-foldr-unfold-�⨫��
   -- **************************************
   import List

   -- **************************************************
   -- �㭪��, ����������� �㭪樮��� unfold �� ᯨ᪠�
   -----------------------------------------------------
   unfold p f g x | p x  = [] 
                  | True = f x : unfold p f g (g x)

   -- ********************************************
   -- (1) fix-foldr-ॠ������ ���᫥��� ���祭��
   --     ��४��ᨢ��� �㭪樨 (\x -> x + 1) 
   ------------------------------------------
   inc = foldr f e [1..]
        where f = \_ -> g
              e = undefined
              g = \f x -> x + 1

   -- �믮���� �䠪�ਭ� �㭪樨 inc � ����稬
   -- ᫥���饥 foldr-�।�⠢�����:

   inc' = foldr f e [1..]
        where f = \_ _ x -> x + 1
              e = undefined

   --------------------------------
   -- �믮���� "����" ���᫥���:
   --
   --  inc' 3 = (foldr f e [1..]) 3 = f 1 (f 2 (f ...)) 3 =
   --         = (\_ _ x -> x+1) 1 (f 2 (f ...)) 3 =
   --         = \x -> x+1) 3 = 4

   -- ********************************************
   -- (2) fix-foldr-ॠ������ �㭪樨 "䠪�ਠ�" 
   -----------------------------------------------
   fact = foldr f e [1..]
        where f    = \_ -> fct
              e    = undefined
              fct = \f n -> if n==0 then 1 else n * f (n-1)

   -- �믮���� �䠪�ਭ� �㭪樨 fact � ����稬 ᫥���饥
   -- foldr-�।�⠢�����:

   fact' = foldr f e [1..]
        where f = \_ f n -> if n==0 then 1 else n * f (n-1)
              e = undefined

   -- ********************************************
   -- (3) fix-foldl-ॠ������ ���᫥��� ���祭��
   --     �㭪樨 "䠪�ਠ�"
   ----------------------------
   fact'' = foldl f e [1..1000]
        where f        = \g _ -> fct' g
              e        = undefined
              fct' g n = if n==0 then 1 else n * g (n-1)

   -- �믮���� �䠪�ਭ� �㭪樨 fact'' � ����稬 ᫥���饥
   -- foldl-�।�⠢�����:

   fact''' = foldl f e [1..10000]
        where f g _ n = if n==0 then 1 else n * g (n-1)
              e       = undefined

   -- *************************************
   -- (4) fix-foldl-ॠ������ �㭪樨 (++)
   -----------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 (++)
   -- �� ������⥪� List:
   --
   -- (++) []     y = y
   -- (++) (x:xs) y = x : (++~) xs y
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   -- (++~) h []     y = y             
   -- (++~) h (x:xs) y = x : h xs y
   --
   -- � ᮧ����� fix-foldl-�।�⠢�����:

   (++~) = foldl f e [1..10000]                      
        where f g _ []     z = z         
              f g _ (x:xs) z = x : g xs z
              e              = undefined

   -- *************************************
   -- (5) fix-foldr-ॠ������ �㭪樨 tail
   ----------------------------------------
   tail' = foldr f e (repeat undefined)
        where f = \_ -> g
              e = undefined 
              g = \f (_:xs) -> xs
  
   -- �믮���� ���⥩訩 �䠪�ਭ� �㭪樨 f:
   --
   -- f = \_ -> g = \_ f (_:xs) -> xs = \_ _ (_:xs) -> xs.
   --
   -- � १���� �������� foldr-�।�⠢�����:

   tail'' = foldr f e (repeat undefined)
        where f = \_ _ (_:xs) -> xs
              e = undefined 

   -- *************************************
   -- (6) fix-foldr-ॠ������ �㭪樨 drop
   --
   --  drop 0 xs     = xs            
   --  drop _ []     = []            
   --  drop n (x:xs) = drop' (n-1) xs
   --
   -------------------------
   drop':: Int -> [a] -> [a]
   drop' = foldr f e [1..]
        where f = \_ -> drop
              e = undefined
              drop g n lst | n==0     = lst
                           | null lst = []
                           | True     = g (n-1) (tail lst)
  
   -- �믮���� �䠪�ਭ� �㭪樨 drop' � ����稬
   -- foldr-�।�⠢�����:

   drop'' = foldr f e [1..]
        where f _ _ 0 lst    = lst               
              f _ _ _ []     = []                
              f _ g n (_:xs) = g (n-1) xs
              e              = undefined

   -- ************************************
   -- (7) fix-foldl-ॠ������ �㭪樨 map
   ---------------------------------------
   map' = foldl f e [1..1000]
        where f = \x _ -> g x
              e = undefined
              g = \f h xs -> [h z | z <- xs]

   -- �믮���� ���⥩訩 �䠪�ਭ� �㭪樨 map'
   -- � ����稬 foldl-�।�⠢�����:

   map'' = foldl f e [1..1000]
        where f _ _ h xs = [h z | z <- xs]
              e          = undefined

   -- ******************************************
   -- (8) fix-foldr-ॠ������ �㭪樨 dropWhile
   -------------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 dropWhile ��
   -- ������⥪� List:
   --
   --  dropWhile p []     = []
   --  dropWhile p (x:xs) = if p x
   --                         then dropWhile p xs
   --                         else x : xs
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --  dropWhile' f p []     = []
   --  dropWhile' f p (x:xs) = if p x
   --                            then f p xs
   --                            else x : xs
   --
   -- � ᮧ����� fix-foldr-�।�⠢�����:

   dropWhile' = foldr f e (repeat undefined)
        where f _ _ _ []     = []            
              f _ g p (x:xs) = if p x        
                                 then g p xs
                                 else x : xs 
              e              = undefined 

   -- **************************************
   -- (9) fix-foldr-ॠ������ �㭪樨 tails
   ---------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 tails ��
   -- ������⥪� List:
   --
   --  tails []     = [[]]
   --  tails (x:xs) = (x : xs) : tails xs
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --   tails' f []     = [[]]
   --   tails' f (x:xs) = (x : xs) : f xs
   --
   -- � ᮧ����� fix-foldr-�।�⠢�����:

   tails' = foldr f e (repeat undefined)
        where f           = \_ -> g
              e           = undefined
              g f1 []     = [[]]
              g f1 (x:xs) = (x : xs) : f1 xs

   -- �����, �믮���� ���⥩訩 �䠪�ਭ� �㭪樨 f
   -- � ����稬 foldr-�।�⠢�����:

   tails'' = foldr f e (repeat undefined)
        where f _ _ []     = [[]]
              f _ g (x:xs) = (x : xs) : g xs
              e            = undefined 

   -- **************************************
   -- (10) fix-foldr-ॠ������ �㭪樨 init
   --------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 init ��
   -- ������⥪� List:
   --
   --   init [x]    = []
   --   init (x:xs) = x : init' xs
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --   init' h [x]    = []
   --   init' h (x:xs) = x : h xs
   --
   -- � ᮧ����� fix-foldr-�।�⠢�����:

   init' = foldr f e (repeat undefined)
        where f _ _ [x]    = []      
              f _ h (x:xs) = x : h xs
              e            = undefined

   -- **************************************
   -- (11) fix-foldl-ॠ������ �㭪樨 init
   --------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 init ��
   -- ������⥪� List:
   --
   --   init [x]    = []
   --   init (x:xs) = x : init' xs
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --   init' h [x]    = []
   --   init' h (x:xs) = x : h xs
   --
   -- � ᮧ����� fix-foldl-�।�⠢�����:

   init'' = foldl f e [1..1000]
        where f _ _ [_]    = []      
              f h _ (x:xs) = x : h xs
              e            = undefined

   -- ****************************************
   -- (12) fix-foldr-ॠ������ �㭪樨 delete
   --      �� ����� List
   --------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 delete:
   --
   -- delete :: Eq a => a -> [a] -> [a]
   -- delete _ []     = []
   -- delete x (y:ys) = if x==y 
   --                     then ys 
   --                     else y : delete x ys
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   -- delete' h _ []     = []
   -- delete' h x (y:ys) = if x==y 
   --                        then ys 
   --                        else y : h x ys
   --
   -- � ᮧ����� fix-foldr-�।�⠢�����:

   delete' :: Eq a => a -> [a] -> [a]
   delete' = foldr f e [1..]
        where f _ _ _ []     = []               
              f _ h x (y:ys) = if x==y
                                 then ys
                                 else y : h x ys
              e              = undefined

   -- ***************************************
   -- (13) fix-foldr-ॠ������ �㭪樨 scanr
   -------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 scanr:
   --
   --  scanr :: (a -> b -> b) -> b -> [a] -> [b]
   --  scanr f q0 []     = [q0]
   --  scanr f q0 (x:xs) = f x q : qs
   --     where qs@(q:_) = scanr f q0 xs
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --  scanr' h f q0 []     = [q0]
   --  scanr' h f q0 (x:xs) = f x q : qs
   --     where qs@(q:_) = h f q0 xs
   --
   -- � ᮧ����� fix-foldr-�।�⠢�����:

   scanr' = foldr f e [1..]                          
        where f _ _ _  q0 []     = [q0]       
              f _ h f' q0 (x:xs) = f' x q : qs
                where qs@(q:_) = h f' q0 xs
              e                  = undefined

   -- ***************************************
   -- (14) fix-foldr-ॠ������ �㭪樨 scanl
   -------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 scanl:
   --
   --  scanl :: (a -> b -> a) -> a -> [b] -> [a]
   --  scanl f q xs = q : (case xs of
   --                       []   -> []
   --                       x:xs -> scanl f (f q x) xs)
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --  scanl' h f q xs = q : (case xs of
   --                          []   -> []
   --                          x:xs -> h f (f q x) xs)
   --
   -- � ᮧ����� fix-foldr-�।�⠢�����:

   scanl'= foldr f e [1..]                          
        where f _ h f' q xs = q : (case xs of
                                    []   -> []
                                    x:xs -> h f' (f' q x) xs)
              e             = undefined

   -- ***************************************
   -- (15) fix-foldl-ॠ������ �㭪樨 foldr
   -------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 foldr:
   --
   --  foldr :: (a -> b -> b) -> b -> [a] -> b
   --  foldr f e []     = e
   --  foldr f e (x:xs) = x `f` foldr f e xs
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --  foldr' h f e []     = e                 
   --  foldr' h f e (x:xs) = f x (h f e xs)
   --
   -- � ᮧ����� ��� ��ਠ�� fix-foldl-�।�⠢�����:

   foldr' = foldl f e [1..500]                   
        where f _ _ _ e []     = e               
              f h _ g e (x:xs) = g x (h g e xs)
              e                = undefined
   ---------------------------------------------------------
   foldr'' f e lst = foldl f' e' [1..1 + length lst] f e lst
        where f' _ _ _ e' []     = e'
              f' h _ g e' (x:xs) = g x (h g e' xs)
              e'                 = undefined

   -- *****************************************
   -- (16) fix-foldr-ॠ������ �㭪樨 zipWith
   ---------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 zipWith:
   --
   --  zipWith z (a:as) (b:bs) = z a b : zipWith z as bs
   --  zipWith _ _      _      = []
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --  zipWith' h z (a:as) (b:bs) = z a b : h z as bs
   --  zipWith' h _ _      _      = []
   --
   -- � ᮧ����� fix-foldr-�।�⠢�����:

   zipWith' = foldr f e [1..]                          
       where f _ h z (a:as) (b:bs) = z a b : h z as bs
             f _ _ _ _      _      = []               
             e                     = undefined

   -- *************************************
   -- (17) fix-foldl-ॠ������ �㭪樨 zip
   -----------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 zip:
   --
   --  zip :: [a] -> [b] -> [(a,b)]
   --  zip xs []         = []
   --  zip [] ys         = []
   --  zip (x:xs) (y:ys) = (x,y) : zip xs ys
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   --  zip' h xs     []     = []
   --  zip' h []     ys     = []
   --  zip' h (x:xs) (y:ys) = (x,y) : h xs ys
   --
   -- � ᮧ����� fix-foldl-�।�⠢�����:

   zip' l1 l2 = foldl f e [1..1+max (length l1) (length l2)] l1 l2                  
        where f _ _ _  []         = []               
              f _ _ [] _          = []              
              f h _ (x:xs) (y:ys) = (x,y) : h xs ys 
              e                   = undefined

   -- ****************************************
   -- (18) fix-foldl-ॠ������ �㭪樨 delete
   --      �� ����� List
   --------------------------------------------
   -- ��ᯮ��㥬�� ॠ����樥� �㭪樨 delete:
   --
   -- delete :: Eq a => a -> [a] -> [a]
   -- delete _ []     = []
   -- delete x (y:ys) = if x==y 
   --                     then ys 
   --                     else y : delete x ys
   --
   -- �� ���ன ����ந� ��४��ᨢ��� "����⮢��"
   --
   -- delete' h _ []     = []
   -- delete' h x (y:ys) = if x==y 
   --                        then ys 
   --                        else y : h x ys
   --
   -- � ᮧ����� fix-foldl-�।�⠢�����:

   delete'' :: Eq a => a -> [a] -> [a]
   delete'' = foldl f e [1..1001]                   
       where f _ _ _ []     = []               
             f h _ x (y:ys) = if x==y          
                                then ys        
                                else y : h x ys
             e              = undefined

   -- **************************************
   -- (19) fix-unfold-ॠ������ �㭪樨 map
   --------------------------------------------------
   -- ��ᯮ�짮������ unfold-ॠ����樥� ���������
   -- ����������� �窨 � ��४��ᨢ��� "����⮢���"
   --
   -- g = \f h xs -> [h z | z <- xs],
   --
   -- ����稬:

   map''' = last $ [e] ++ unfold p f' g' ([1..100],e)  
       where f  x _ h xs = [h z | z <- xs]
             e           = undefined
             p  (x,y)    = null x
             f' (x,y)    = f y (head x)
             g' (x,y)    = (tail x, f' (x,y))

   -- ********************************************
   -- (20) fix-foldr-unfold-ॠ������ �㭪樨 inc
   ----------------------------------------------------
   inc'' = (foldr f e . unfold p f1 g1) (\f x -> x + 1)
        where f  = ($)
              e  = undefined
              p  = \x -> False
              f1 = id
              g1 = id                     

   -- ********************************************
   -- (21) fix-foldr-unfold-ॠ������ �㭪樨 map
   -----------------------------------------------
   map'''' = (foldr f e . unfold p f1 g1)
                             (\f g xs -> [g x | x <- xs])      
        where f  = ($)
              e  = undefined
              p  = \x -> False
              f1 = id
              g1 = id                     

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------------------------------------
   test1  = inc' 111111111111111111111 == 111111111111111111112 
   test1' = inc'' 111111111111111111111 == 111111111111111111112 
   test2  = map' (\x -> fact x == fct x) [1000..1100]
       where fct n = if n==0 then 1 else n * fct (n-1)
   test3  = fact 3990 
   --------------------------------------------
   test4 = map' (\x -> drop' (fromInt x) lst == 
                       drop'' (fromInt x) lst) lst
       where lst = [0..50]
   test5 = take 10 $ drop' 5 [1..]         -- ����� �ਬ��!
   ---------------------------------------------------------
   test6 = map' (\x -> head x) [[1,2,3],[2,5],[3,7,8,9]] ==
           map'' (\x -> head x) [[1,2,3],[2,5],[3,7,8,9]]
   test7 = map''' (^2) [1..1000] == map'' (^2) [1..1000]
   test8 = map''   (\x -> (x, lst, delete x lst)) lst ==
           map'''' (\x -> (x, lst, delete x lst)) lst
       where lst = [1..90]
   test9 = map'''' (\x -> (x, lst, delete x lst)) lst ==
           map'''  (\x -> (x, lst, delete x lst)) lst
       where lst = [1..90]
   ---------------------------------------------
   test10 = dropWhile' (< 0) [-1,-2,-3,4,-5,3,2] 
              == dropWhile (< 0) [-1,-2,-3,4,-5,3,2] 
   test11 = dropWhile' (< 0) [4,-5,3,2] 
              == dropWhile (< 0) [4,-5,3,2] 
   test12 = ("123" ++~ "123", "" ++~ "")
   test13 = [1..5000] ++~ [1..5000]
   ---------------------------------------------------
   test14   = delete'  1 (replicate 1000 2 ++ [1,1,1])
   test14'  = delete'' 2 [1,1,1,5,4,2,6]
   test14'' = delete'' 1 z 
           where z = replicate 1000 2 ++ [1,1,1]
   ---------------------------------------------
   test15 = (init' [1], init' [1..4]) == 
            (init'' [1], init'' [1..4])
   --------------------------------------------------
   test16 = scanr' (+) 1 [1..5] == scanr (+) 1 [1..5]
   test17 =   scanr' (+) 0 [1,2,3,4,5] == [15,14,12,9,5,0]
           && scanr' (*) 2 [5,6,7]     == [420,84,14,2]
           && scanr' min (-5) [-1,2,-13,-4,-6] 
                                       == [-13,-13,-13,-6,-6,-5]
           && scanr' (-) 0 [1,2,3,4,5] == [3,-2,4,-1,5,0]
   ------------------------------------------------------
   test18 =   scanl' (+) 0 [1,2,3,4,5] == [0,1,3,6,10,15]
           && scanl' (*) 2 [5,6,7]     == [2,10,60,420]
           && scanl' min (-5) [-1,2,-13,-4,-6]
                                       == [-5,-5,-5,-13,-13,-13]
           && scanl' (-) 0 [1..5]      == [0,-1,-3,-6,-10,-15]
   -----------------------------------------------------------
   test19 = foldr' (+) 0 [1..100] == foldr (+) 0 [1..100]
   test20 = foldr' (*) 1 [1..60]  == foldr (*) 1 [1..60]
   test21 =   foldr' (++) [] [[1,2,3],[5,6,7]]
           == foldr  (++) [] [[1,2,3],[5,6,7]]
   test22 = foldr'' (+) 0 [1..2000] == foldr (+) 0 [1..2000]
   ---------------------------------------------------------
   test23 = zip' [1..10] [2..11]
   test24 = zipWith' (+) [1..5] [1..20] ==
            zipWith' (+) [1..5] [1..20]
