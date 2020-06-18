   -- Демонстрация одного из вариантов построения
   -- бинарного дерева поиска
   -- [Bird,1998,p.188-189]
   -- *********************
   import Tree
   -- ********************
   mkTree:: [Int] -> BTree
   mkTree []     = nil
   mkTree (x:xs) = node x (mkTree ys) (mkTree zs)
          where (ys,zs) = partition' (<= x) xs
   ----------------------------------------------------
   partition':: (Int -> Bool) -> [Int] -> ([Int],[Int])
   partition' p xs = (filter p xs, filter (not . p) xs)
   ----------------------------------------------------
   filterBT':: (Int -> Bool) -> BTree -> [Int]
   filterBT' p Nil = []
   filterBT' p (Node x l r) 
         | p x  = [x] ++ filterBT' p l ++ filterBT' p r
         | True = filterBT' p l ++ filterBT' p r

   -- ***************************
   -- Неудачные тестовые примеры:
   --------------------------------
   tree1 = node 10 (node 6 (list 3)
                           (node 7 nil (list 8)))
                   (node 15 (list 12) 
                            (node 18 (list 17) nil))
   -------------------------------------------------
   test1 = outTree $ mkTree [10,6,3,7,8,15,12,18,17]
   test2 = mkTree [10,6,3,7,8,15,12,18,17] ==
           consTree [10,6,3,7,8,15,12,18,17]
   test3 = tree1 == consTree [10,6,3,7,8,15,12,18,17]

