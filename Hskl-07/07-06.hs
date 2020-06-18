   -- ���������� ������஢���� ᡠ����஢�����
   -- (�ॢ����) ���⮪
   -- *******************
   import List

   -- ************************************
   -- �������஢����� (�ॢ�᭠�) ���⪠
   -- � ���ࠢ����� "ᢥ��� ����"
   ------------------------------
   foldb f [x] = x
   foldb f xs  = f (foldb f (take (length xs `div` 2) xs))
                   (foldb f (drop (length xs `div` 2) xs))

   -- ************************************
   -- �������஢����� (�ॢ�᭠�) ���⪠
   -- � ���ࠢ����� "᭨�� �����"
   ------------------------------
   foldb' f [x] = x
   foldb' f xs  = foldb' f (pair f xs)
   -----------------------------------
   pair f []             = []
   pair f [x]            = [x]
   pair f (x1 : x2 : xs) = f x1 x2 : pair f xs

   -- *******************************************
   -- ���������� ॠ����樨 ���஢�� ᫨ﭨ��
   -- � foldb-�⨫�
   ----------------
   sort' [] = []
   sort' xs = foldb merge (map (\z -> [z]) xs)
   -------------------------------------------
   merge:: Ord a => [a] -> [a] -> [a]
   merge [] ys = ys
   merge xs [] = xs
   merge (x:xs) (y:ys) | x<=y = x : merge   xs   (y:ys)
                       | True = y : merge (x:xs)   ys

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test1 = foldb  (+) [1,2,3,4,5]
   test2 = foldb' (+) [1,2,3,4,5]
   ------------------------------
   test3 = sort' [1500,1499..1]
   test4 = sort' z == sort z
        where z = [1000,999..1]
