                        �2����� ��� ��������
       (�1�ᥭ��� ᥬ���� 2016-2017 ��. ���, ��, ���� �����0)

   �21.�0 ��⠭���� �᫮��� �����.
��1.0

   zamena :: (Integral a, Floating b) => [(a,b)] -> [(a,b)]
   zamena = map (\(k,a) -> (k, a + fromIntegral k))

��1.2
   �22.�0 ������ ����� ������� ᯨ᪠ �������� �᫮ ࠧ.
��1.0

   > f "abc" 3
   "aaabbbccc"

��1.2
   �23.�0 ������ ����� n-� ������� ᯨ᪠.
��1.0

   > f "abcdefghik" 3
   "abdeghk"

��1.2
   �24.�0 �뤥��� ���ᯨ᮪ � n-�� �� k-� ����� [n;k).
��1.0

   > f "abcdefghik" 2 5
   "cde"

��1.2
   �25.�0 ������ 横������ ���� ᯨ᪠ �����.
��1.0

   > f "abcdefghik" (-2)
   "ikabcdefgh"

��1.2
   �26.�0 ������ n-� ������� �� ᯨ᪠, ���� ��� � ᯨ᮪.
��1.0

   > f 1 "abcd"
   ('b',"acd")

��1.2
   �27.�0 ������ �� ��⠭�� �� ������⮢ ��������� ᯨ᪠ �� n ���-
���⮢.
��1.0

   > f 3 "abcde"
   ["abc","abd","acd","bcd","abe","ace","bce","ade","bde","cde"]

��1.2
   �28.�0 �ᯮ���� �ࠢ�� �����, ������ �㭪��, ������������� ��
ᯨ᪠ ��ப ��ப�, ࠧ������ �����묨.
��1.0

   > f ["ab","cde","fgh"]
   "ab,cde,fgh"

��1.2
.
   �29.�0 ������ ��� ᫥���饩 �㭪樨:

   -- �㭪�� �������� (��� ����襭�� 㯮�冷祭����) �᫮-
   -- ��� ���� (x,y) � ���樠⨢�� ᯨ᮪ alist, ����  ���-
   -- ண� �ᯮ������ � ���浪� �뢠��� ������.
   -- �᫨ ��� (x,y) ���� � ᯨ᪥, � ��� �� ����������
   -------------------------------------------------------
   acons2:: Integer -> Double -> [(Integer,Double)]
                              -> [(Integer,Double)]
   acons2 x y alist 
         | null alist          = [(x,y)]
         | y<snd (head alist)  = head alist : 
                                 acons2 x y (tail alist)
         | y>snd (head alist)  = (x,y) : alist
         | x==fst (head alist) = acons2 x y (tail alist)
         | True                = head alist : 
                                 acons2 x y (tail alist)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   -----------------------------------------------
   test =   acons2 3 33.0 []         == [(3,33.0)]
         && acons2 2 22.0 [(2,22.0)] == [(2,22.0)]
         && acons2 5 55.0 [(44,4.0),(45,3.0),(3,2.0)]
              == [(5,55.0),(44,4.0),(45,3.0),(3,2.0)]
         && acons2 3  5.0 [(2,22.0),(4,11.0)]
                      == [(2,22.0),(4,11.0),(3,5.0)]
         && acons2 3 15.0 [(2,22.0),         (4,11.0)]
                       == [(2,22.0),(3,15.0),(4,11.0)]
         && acons2 3 33.0 [(3,33.0),(2,22.0),(4,11.0)]
                       == [(3,33.0),(2,22.0),(4,11.0)]
         && acons2 2 22.0 [(3,33.0),(2,22.0),(4,11.0)]
                       == [(3,33.0),(2,22.0),(4,11.0)]
         ---------------------------------------------
         && acons2 3  3.0 [(3,7.0),(3,5.0)]
                       == [(3,7.0),(3,5.0),(3,3.0)]
         && acons2 3  3.0 [(3,7.0),(3,4.0),        (3,1.0)]
                       == [(3,7.0),(3,4.0),(3,3.0),(3,1.0)]
         
         && acons2 3  3.0 [(3,3.0),(3,3.0),(3,3.0)]
                       == [(3,3.0)]
         ------------------------------------
         -- ����஥��� ���樠⨢���� ᯨ᪠,
         -- 㯮�冷祭���� �� �뢠��� ������
         ------------------------------------
         && acons2 1 1.0
             (acons2 10 1.0
               (acons2 10 2.0
                 (acons2 5 2.0
                   (acons2 4 1.0
                     (acons2 1 3.0 [])))))
         == [(1,3.0),(5,2.0),(10,2.0),(4,1.0),(10,1.0),(1,1.0)]
   ------------------------------------------------------------
   test1 =   zamena [(1,0.1),(2,0.4),(3,0.5)]
                 == [(1,1.1),(2,2.4),(3,3.5)]
          && zamena [(1,0.1),(5,0.4),(3,0.5)]
                 == [(1,1.1),(5,5.4),(3,3.5)]

   �210.�0 ������ ��� �㭪樨

   �2acons2 x y alist�0,

����� �������� �������� ����� (!) ���� ������⮢ (x,y),  ��� x �
y - �᫠,  � �᫮��� ���樠⨢�� ᯨ᮪ alist,  ����  ������⮢
���ண� �ᯮ������ � ���浪� �1�뢠����0 ���祭�� �� �1�������0.
   ���ਬ��:

   -- ���������� � ��砫�
   > acons2 3 33.0 []
   [(3,33.0)]

   -- ���������� � �����
   > acons2 5 55.0 [(44,4.0),(45,3.0),(3,2.0)]
   [(5,55.0),(44,4.0),(45,3.0),(3,2.0)]

   -- ���������� � �।���
   > acons2 3 15.2 [(2,22.1),(4,11.45)]
   [(2,22.1),(3,15.2),(4,11.45)]

   -- ���������� ����, ����� 㦥 ���� � ᯨ᪥
   > acons2 2 22.0 [(3,33.0),(2,22.0),(4,11.0)]
   [(3,33.0),(2,22.0),(4,11.0)]


   �211.�0 ������ �����祭�� ᫥���饩 �㭪樨:

   {-# LANGUAGE RankNTypes #-}
   {-# LANGUAGE ScopedTypeVariables #-}

   test17 :: forall a. forall b. forall g.
                         ((a -> b) -> (a -> g)) -> (a -> (b -> g))
   test17 = \(f :: (a -> b) -> (a -> g)) (x :: a) (y :: b)
                                                  -> f (const y) x
