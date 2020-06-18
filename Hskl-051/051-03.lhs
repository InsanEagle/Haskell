>   import List (sort,groupBy,(\\),nub,insert)
>   import Random

-- >   import Data.Char

      Решим следующую задачу: написать функцию, вычеркивающую
   из заданного слова word буквы, которые встречаются в нём n
   раз. Использование функционалов обязательно!
      Заметим, что  функционалов очень много,  поэтому первой
   проблемой является их выбор (понятно,  что функционалы map
   и filter придётся использовать).  Предположим вначале, что
   буквы в слове отсортированы; это легко сделать так (word -
   это входное слово):
      sort word
      Далее, вспомним функционал, позволяющий выполнить груп-
   пировку совпадающих букв в слове word:
      groupBy (==) $ sort word
      Теперь, например, получим для word="asaswasaszewz":
      "asaswasaszewz" -> ["aaaa","e","ssss","ww","zz"]
      Оставим только "нужные" буквы с помощью фильтрации:
      filter (\x -> length x==n)
             (groupBy (==) $ sort word)
      Например, для n=2 получим:
      ["aaaa","e","ssss","ww","zz"] -> ["ww","zz"]
      Удаляем лишние буквы:
      map (\y -> head y)
          (filter (\x -> length x==n)
                  (groupBy (==) $ sort word))
   и для сокращения воспользуемся замыканием:
      where z = map (\y -> head y)
                    (filter (\x -> length x==n)
                            (groupBy (==) $ sort word))
      Итак, ["ww","zz"] -> ['w','z'].
      Остаётся удалить "буквы", входящие в список z, из слова
   word.
      Заметим, что существует редко используемая функция (\\)
   из библиотеки List, которая осуществляет это удаление; од-
   нако предварительно приходится выполнить следующие  преоб-
   разования:
      ['w','z'] -> [['w','z'],['w','z']] -> ['w','z','w','z']
                -> "wzwz"
      Наконец, функция (\\) позволяет получить:
      (\\) "asaswasaszewz" "wzwz" -> "asasasase"
      Итоговый код:

>   bca word n = (\\) word (concat (replicate n z))
>           where z = map (\y -> head y)
>                         (filter (\x -> length x==n)
>                                 (groupBy (==) $ sort word))

    Остаётся провести примитивное тестирование:

>   test =   bca "aaaasssssweqew" 4 == "sssssweqew"
>         && bca "aaaasssssweqew" 5 == "aaaaweqew"
>         && bca "aaaasssssweqew" 2 == "aaaasssssq"
>         && bca "aaaasssssweqew" 1 == "aaaasssssweew"
>         && bca "aaaasssssweqew" 0 == "aaaasssssweqew"

      Выполним случайное тестирование;  для этого  произведём
   генерацию k случайных натуральных чисел от 97 до 122 (коды
   строчных латинских букв от 'a' до 'z'), а затем преобразу-
   ем  эти  числа в символы (201 - случайное натуральное чис-
   ло):

>   wrd k = map chr
>             (take k $ randomRs (97,122) (mkStdGen 201):: [Int])

>   test1 = (w,sort w,sort $ bca w 2,2)
>       where w = wrd 20  

      Остаётся генерировать случайным образом строки (для простоты
   одинаковой длины) для тестирования функции bca:

>   test2 = map (\k -> map chr
>                   (take 20 $ randomRs (97,122) (mkStdGen k):: [Int]))
>               [1,11..200000]

   и провести тестирование:

>   test3 = map (\w -> (w,sort w,sort $ bca w 1,1)) test2

      К сожалению, для проверки результатов тестирования необходимо
   присутствие программиста, который анализирирует успешность тестов.
   Поэтому для дальнейшей автоматизации тестирования воспользуемся
   оракулом тестов, в качестве которого будет использована другая 
   реализация функции bca:

>   -- Функция, вычеркивающая из заданного слова
>   -- буквы, которые встречаются в нём n раз
>   -- (неэффективная реализация)
>   ----------------------------------------------
>   result n word = vsp3 (vsp2 n (vsp1 word)) word
>       where vsp2 n lst = map (\(a,b) -> if b==n then a else '#')
>                              lst
>             vsp3 lst word 
>                 | null word            = []
>                 | elem (head word) lst = vsp3 lst (tail word)
>                 | True                 = head word : 
>                                          vsp3 lst (tail word)
>   -----------------------------------------------------------
>   vsp1 word = map (\x -> (x,poisk x word)) (nub word)
>          where poisk x word 
>                  | null word    = 0
>                  | head word==x = 1 + poisk x (tail word)
>                  | True         = poisk x (tail word)

   Итоговый вид функции для тестирования таков (в GHCi):

>   test4 = map (\w -> (w,bca w 1,1)) lst ==
>           map (\w -> (w,result 1 w,1)) lst
>       where lst = test2 

   Результат тестирования: True.
   Проблема: а что, если функция test4 вернёт False?

>   test5 | num /= length z1 = z1 !! num
>         | True              = ""
>       where num = length $ takeWhile (==True) fun 
>             fun = zipWith (==) (map (\w -> (w,bca w 1)) z1)
>                                (map (\w -> (w,result 1 w)) z2)
>             lst = test2 
>             z1  = insert "abcdefgh" lst
>             z2  = insert "cab" lst 