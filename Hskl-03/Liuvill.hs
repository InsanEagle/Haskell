   import List
   -- ***********************************************
   -- �������஢���� �࠭�業���⭮�� �᫠ ��㢨���
   --------------------------------------------------
   abc input = map length $ groupBy (==) $ concat $
                   map (\x -> [1] ++ replicate (fct (x+1)-fct x -1) 0)
                       [1..input]
   ------------------------------
   fct n = product [2..n]

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test input = concat $
                 map (\x -> [1] ++ replicate (fct (x+1)-fct x -1) 0)
                     [1..input]
