   -- Демонстрация варианта реализации удаления узла
   -- из бинарного дерева поиска
   -- [Роганова,2002,с.192-194]
   -- *************************
   import Tree
   ----------------------------------------------------- 
   -- Функция, удаляющая узел из бинарного дерева поиска
   -----------------------------------------------------
   deleteTree:: Int -> BTree -> BTree
   deleteTree e Nil          = nil
   deleteTree e (Node x l r) | e<x  = node x (deleteTree e l) r
                             | e==x = join l r
                             | e>x  = node x l (deleteTree e r)
   ------------------------------------------------------------
   -- Вспомогательная функция для функции deleteTree
   -------------------------------------------------
   join:: BTree -> BTree -> BTree
   join Nil b2 = b2
   join b1 b2  = node x b1' b2 
       where (x,b1') = largest b1
   -------------------------------------------
   -- Вспомогательная функция для функции join
   -------------------------------------------
   largest:: BTree -> (Int,BTree)
   largest (Node x b1 Nil) = (x,b1)
   largest (Node x b1 b2)  = (y,node x b1 b2')
       where (y,b2') =largest b2
   -- ***************************
   -- Неудачные тестовые примеры:
   ------------------------------------
   test1 = deleteTree 5 (list 5) == Nil
        && deleteTree 10 
              (node 10 (node 4 nil (node 6 (list 5)
                                           (node 8 (list 7)
                                                   (list 9))))
                       (list 12))
            == node 9 (node 4 nil (node 6 (list 5)                    
                                          (node 8 (list 7) nil))) 
                      (list 12)
        -----------------------------
        && deleteTree 10 (Node 10 Nil
                               (Node 14 (Node 12 (Node 11 Nil Nil)
                                                 (Node 13 Nil Nil))
                                        (Node 15 Nil Nil)))
            == Node 14 (Node 12 (Node 11 Nil Nil)
                                (Node 13 Nil Nil))
                       (Node 15 Nil Nil)
        ------------------------------------------------
        && deleteTree 2 (Node 4 (Node 2 (Node 1 Nil Nil)
                                        (Node 3 Nil Nil))
                                (Node 6 Nil Nil))
            == Node 4 (Node 1 Nil
                              (Node 3 Nil Nil))
                      (Node 6 Nil Nil)
        ------------------------------------------------
        && deleteTree 4 (Node 4 (Node 2 (Node 1 Nil Nil)
                                        (Node 3 Nil Nil))
                                (Node 6 Nil Nil))
            == Node 3 (Node 2 (Node 1 Nil Nil) Nil)
                              (Node 6 Nil Nil)
        --------------------------------------
        && deleteTree 7 
               (Node 5 Nil
                       (Node 10 (Node 7 (Node 6 Nil Nil)
                                        (Node 8 Nil
                                               (Node 9 Nil Nil)))
                                Nil))
            == Node 5 Nil
                      (Node 10 (Node 6 Nil
                                       (Node 8 Nil (Node 9 Nil Nil)))
                                 Nil)
   -----------------------------------------------------------
   -- Сравнение результатов работы функций delete и deleteTree
   -----------------------------------------------------------
   test2 = deleteTree 10 
              (Node 10 (Node 4 Nil
                               (Node 6 (Node 5 Nil Nil)
                                       (Node 8 (Node 7 Nil Nil)
                                               (Node 9 Nil Nil))))
                       Nil)
   test3 = delete 10 
              (Node 10 (Node 4 Nil
                               (Node 6 (Node 5 Nil Nil)
                                       (Node 8 (Node 7 Nil Nil)
                                               (Node 9 Nil Nil))))
                       Nil)
