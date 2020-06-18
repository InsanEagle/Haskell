-- Задачи. Драгун Валентин 
   import List
   import Prelude (hiding, unwords)

-- №1
-- Установите условие задачи.
   zamena :: (Integral a, Floating b) => [(a,b)] -> [(a,b)]
   zamena = map (\(k,a) -> (k, a + fromIntegral k))
-- Прибавляет число k к a во 2 элементе списка

-----------------------------------------------------------------
-- №2 Повторите каждый элемент списка заданное число раз.
   f2 n lst = concat (replicate n lst)
   f21 [] n = []
   f21 (x:xs) n = take n (repeat x) ++ f21 xs n
  
-- №3
   rem :: Eq a => Int -> [a] -> [a] 
   rem n x = rem (x !! n) x
-- Удачные тестовые примеры    
   test1 = f2 3 "abc" == "abcabcabc"
   test21 = f21 [888..900] 3 == [888,888,888,889,889,889,890,890,890,891,891,891,892,892,892,893,893,893,894,894,894,895,895,895,896,896,896,897,897,897,898,898,898,899,899,899,900,900,900]


-- №4 Выделите подсписок с n-го по k-ый номер [n;k). f "abcdefghik" 2 5 ----> "cde"
   f4 n x str = drop n (take x str)
-- test4 = 2 5 "abcdefghik" == "cde"

-- 5 Задайте циклическую ротацию списка влево.
   rotate :: Int -> [a] -> [a]
   rotate n [] = []
   rotate 0 x = x
   rotate n (x:xs) = rotate (n-1) xs ++ [x]

   test5 = rotate (-2) "abcdefghik" == "kihgfedcba"
   test6 = rotate (-1) ['a'..'z']   ==  "zyxwvutsrqponmlkjihgfedcba"