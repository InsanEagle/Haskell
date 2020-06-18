   -- Библиотека для простейшей работы с произвольными
   -- бинарными деревьями
   -- *******************
   import Tree
   --------------------------------------------------------
   -- Функция для построения произвольного бинарного дерева
   -- tree с помощью списка списков вида:
   --
   --  [[a ,b ,0],[a ,b ,1],[a ,b ,1],[a ,b ,0],...,[a ,b ,1]],
   --     1  1      2  2      3  3      4  4          n  n
   --    │  │  │
   --    │  │  └ Направление прикрепления (0 - левое поддерево,
   --    │  │                             (1 - правое поддерево)
   --    │  └─── Отец добавляемого элемента
   --    └────── Добавляемый элемент
   --
   -- Список [a ,a ,...,a ] строится с помощью обхода  бинарного
   --          1  2      n
   -- дерева в ширину
   ------------------------------------
   construct lst tree | null lst = tree
                      | True     = construct (tail lst)
                                             (addTree'' (head lst)
                                                        tree)
   ---------------------------------------------------------------
   -- Функция, добавляющая вершину в произвольное бинарное дерево;
   -- lst - список, содержащий три элемента:
   --   (1) добавляемая вершина;
   --   (2) отец добавляемой вершины;
   --   (3) направление прикрепления (0 - левое поддерево,
   --                                (1 - правое поддерево)
   --------------------------------------------------------
   addTree'' lst tree 
                  | isEmpty tree = node (head lst) nil nil
                  | head (tail lst)==root tree && last lst==0
                                 = node (root tree)
                                        (node (head lst) nil nil)
                                        (right tree)
                  | head (tail lst)==root tree && last lst==1
                                 = node (root tree)
                                        (left tree)
                                        (node (head lst) nil nil)
                  | isNode (search' (left tree) (head (tail lst)))
                                 = node (root tree)
                                        (addTree'' lst (left tree))
                                        (right tree)
                  | isNode (search' (right tree) (head (tail lst)))
                                 = node (root tree)
                                        (left tree)
                                        (addTree'' lst (right tree))
                  | True         = error "Неверные исходные данные"
   ----------------------------------------------------------------
   -- Функция для поиска элемента x в произвольном бинарном
   -- дереве tree (неэффективная функция!)
   ---------------------------------------
   search':: BTree -> Int -> BTree 
   search' tree x | null $ abc tree x= nil
                  | True             = head $ abc tree x
      where abc tree x | isEmpty tree = []
                       | root tree==x = [tree]
                       | True         = abc (left tree) x ++
                                        abc (right tree) x
   -- ****************************************************
   -- Неудачные тестовые примеры:
   ------------------------------
   tree1 = node 10 (node 4 nil
                         (node 6 (list 5)
                                 (node 8 (list 7) (list 9))))
                   (list 12)
   -------------------------
   test1  = search' tree1 10  
   test2  = search' tree1  4  
   test3  = search' tree1 12  
   test4  = search' tree1  6
   test5  = search' tree1  5
   test6  = search' tree1  8
   test7  = search' tree1  7
   test8  = search' tree1  9
   test81 = search' tree1 14
   -------------------------
   test9 = do
             outTree tree1; putStr "\n"
             outTree (addTree'' [100,12,0] tree1)
   ---------------------------------------------------------
   tree2 = construct [[1,0,0],        -- Представление корня
                      [2,1,0],[3,1,1],                
                      [4,2,0],[5,2,1],[6,3,0],[7,3,1],
                      [8,4,0],[9,4,1]]                
                     nil
   ---------------------------------------------------------                          
   tree3 = construct [[13,0,0],       -- Представление корня
                      [10,13,0],[9,13,1],                
                      [4,10,0],[3,10,1],[7,9,0],[8,9,1],
                      [2,4,0],[1,4,1],[2,3,0]]                
                     nil
   test9' = outTree tree3                         
   ----------------------
   test10 = do
              outTree tree2; putStr "\n"
              outTree (construct [[11,8,1],[12,9,0],[13,9,1]]
                                 tree2)
