   -- Неудачные тестовые примеры для проверки функций библиотеки
   -- для работы с числовыми бинарными деревьями поиска
   ----------------------------------------------------
   import Tree

   test =   testRoot && testLeft && testRight 
         && testAddTree
         && testConsTree && testConsTree' && testConsTree''
         && testSearch && testSearchBool
         && testRightList && testLeftList && testDelete 
         && testTop && testtopNode && testNode
         && testNList 
         && testWay && testEqualTree && testTCopy 
         && testKLP_Obh && testLPK_Obh && testLKP_Obh
   ---------------------------------------------------
   -- root - функция, возвращающая корень дерева BTree
   ---------------------------------------------------------
   testRoot =   root (list 2)                           == 2
             && root (node 5 (node 3 (list 1) (list 4)) 
                             (list 7))                  == 5
             && root (node 1 (node 2 (list 3) nil) nil) == 1
             && root (node 4 (node 2 (list 1) (list 3))
                             (list 6))                  == 4
             && root (node 5 (node 3 (list 1) (list 4))
                             (node 7 (list 6) nil))     == 5
   ------------------------------------------------------------
   -- left - функция, возвращающая левое поддерево дерева BTree
   ------------------------------------------------------------
   testLeft =   left (list 5) == nil
             && left (node 4 nil (node 6 (list 5) 
                                       (node 25 nil (list 28))))
                              == nil
             && left (node 5 (node 3 (list 1) nil) nil)
                              == (node 3 (list 1) nil)
             && left (node 4 (node 2 (list 1) (list 3)) (list 6))
                              == (node 2 (list 1) (list 3))
   --------------------------------------------------------------
   -- right - функция, возвращающая правое поддерево дерева BTree
   --------------------------------------------------------------
   testRight = right (node 22 nil nil) == nil
            && ((right (node 5 (node 2 (node 4 (node 3 nil nil)
                                             nil) nil) nil)) == nil)
            && ((right (node 5 nil (node 6 (node 7 nil nil) nil)))
                 == (node 6 (node 7 nil nil) nil))
            && ((right (node 5 (node 3 (node 1 nil nil)
                                       (node 4 nil nil))
                               (node 7 (node 6 nil nil) nil))) ==
               (node 7 (node 6 nil nil) nil))
   ----------------------------------------------------------------
   -- addTree - функция добавления элемента a в дерево поиска BTree
   ----------------------------------------------------------------
   testAddTree =   addTree 2 nil == node 2 nil nil
                && addTree (-3) (node 5 nil nil)
                       == node 5 (node (-3) nil nil) nil
                && addTree 6 (node 5 (node (-2) (node 3 nil nil)
                                                nil) nil)
                       == node 5 (node (-2) (node 3 nil nil) nil)
                                 (node 6 nil nil)
              && addTree 4 (node 6 (node 5 nil nil)
                                   (node 9 (node 7 nil nil)
                                           (node 18 nil nil)))
                       == node 6 (node 5 (node 4 nil nil) nil)
                                 (node 9 (node 7 nil nil)
                                         (node 18 nil nil))
              && addTree 7 (node 5 (node 3 (node 1 nil nil)
                                           (node 4 nil nil))
                                   (node 8 (node 6 nil nil) nil))
                       == node 5 (node 3 (node 1 nil nil)
                                         (node 4 nil nil))
                                 (node 8 (node 6 nil
                                                 (node 7 nil nil))
                                         nil)
   ----------------------------------------------------------------
   -- consTree - функция конструирования бинарного дерева поиска из
   --         элементов списка, поступающих в порядке их следования
   ----------------------------------------------------------------
   testConsTree =   consTree [] == nil
                 && consTree [1,2,3]
                                == node 1 nil (node 2 nil (list 3))
                 && consTree [3,1,2]
                                == node 3 (node 1 nil (list 2)) nil
                 && consTree [1,3,2,6,4]
                                == node 1 nil (node 3 (list 2) 
                                              (node 6 (list 4) nil))
                 && consTree [5,3,7,1]
                                == node 5 (node 3 (list 1) nil)
                                          (list 7)
   -----------------------------------------------
   testConsTree'' =    consTree'' []     == nil
                   && consTree'' [5]     == list 5
                   && consTree'' [3,2,1] == node 3 (node 2 (list 1)
                                                           nil) nil
                   && consTree'' [2,1,3] == node 2 (list 1) (list 3)
                   && consTree'' [4,6,2,3,1]
                                         == node 4 (node 2 (list 1)
                                                           (list 3))
                                                   (list 6)
   --------------------------------------------------------
   testConsTree' =    consTree' []     == nil
                  && consTree' [5]     == list 5
                  && consTree' [1,2,3] == node 3 (node 2 (list 1)
                                                         nil) nil
                  && consTree' [3,1,2] == node 2 (list 1) (list 3)
                  && consTree' [1,3,2,6,4]
                                       == node 4 (node 2 (list 1)
                                                         (list 3))
                                                 (list 6)
   ------------------------------------------------------------
   -- search - функция поиска элемента в бинарном дереве поиска
   ------------------------------------------------------------
   testSearch = search 18 (node 4 (node 2 nil nil)
                                  (node 6 nil nil)) == nil
             && ((search 5 (node 5 (node 3 (node 1 nil nil)
                                           (node 4 nil nil))
                                   (node 7 (node 6 nil nil) nil)))
                 == (node 5 (node 3 (node 1 nil nil)
                                    (node 4 nil nil))
                            (node 7 (node 6 nil nil) nil)))
             && ((search 3 (node 5 (node 3 (node 1 nil nil)
                                           (node 4 nil nil))
                                   (node 7 (node 6 nil nil) nil)))
                 == (node 3 (node 1 nil nil) (node 4 nil nil)) )
             && ((search 1 (node 4 (node 2 (node 1 nil nil)
                                           (node 3 nil nil))
                                   (node 6 nil nil))) ==
                 (node 1 nil nil))
             && ((search 6 (node 5 (node 3 (node 1 nil nil)
                                           (node 4 nil nil))
                                   (node 7 (node 6 nil nil) nil)))
                 == (node 6 nil nil))
   -------------------------------------------------------
   testSearchBool = searchBool 18 (node 4 (node 2 nil nil)
                                          (node 6 nil nil))
                      == False
             && searchBool 5 (node 5 (node 3 (node 1 nil nil)
                                             (node 4 nil nil))
                                     (node 7 (node 6 nil nil) nil))
                      == True

             && searchBool 3 (node 5 (node 3 (node 1 nil nil)
                                             (node 4 nil nil))
                                     (node 7 (node 6 nil nil) nil))
                      == True

             && searchBool 1 (node 4 (node 2 (node 1 nil nil)
                                             (node 3 nil nil))
                                     (node 6 nil nil))
                      == True
             && searchBool 6 (node 5 (node 3 (node 1 nil nil)
                                             (node 4 nil nil))
                                     (node 7 (node 6 nil nil) nil))
                      == True
   -------------------------------------------------------------
   -- rightList - функция, возвращающая самый правый лист дерева
   -------------------------------------------------------------
   testRightList = ((rightList (node 5 (node 2 (node 3 nil nil)
                                               nil) nil)) == 3)
                && ((rightList (node 5 (node 2 nil nil)
                                       (node 6 (node 9 nil nil)
                                               nil))) == 9)
                && ((rightList
                       (node 5 (node 2 nil nil)
                               (node 8 (node 7 nil
                                               (node 9 nil nil))
                                       nil))) == 9)
                && ((rightList 
                    (node 8 (node 6 (node 1 nil nil) nil)
                            (node 10 nil
                                     (node 14 (node 12 nil nil)
                                              (node 22 nil nil)))))
                    == 22)
   -----------------------------------------------------------
   -- leftList - функция, возвращающая самый левый лист дерева
   -----------------------------------------------------------
   testLeftList = ((leftList 
                   (node 5 nil (node 6 (node 7 (node 8 nil nil) nil)
                                       nil))) == 8)
               && ((leftList
                      (node 6 (node 5 (node 4 (node 2 nil nil)
                                              nil)
                                      nil) nil)) == 2)
               && ((leftList (node 4 (node 2 nil (node 3 nil nil))
                                     (node 6 nil nil))) == 3)
               && ((leftList (node 5 (node 3 (node 5 nil nil)
                                             (node 4 nil nil))
                                     (node 7 (node 6 nil nil)
                                             nil))) == 5)
   -----------------------------------------------------------
   -- delete - функция, удаляющая узел бинарного дерева поиска
   -----------------------------------------------------------
   testDelete = delete 5 (node 5 nil nil) == nil
             && ((delete 10 
                 (node 10 (node 4 nil
                                  (node 6 (node 5 nil nil)
                                          (node 8 (node 7 nil nil)
                                                  (node 9 nil nil))
                                  )) nil)) ==
                 (node 4 nil (node 6 (node 5 nil nil)
                                     (node 8 (node 7 nil nil)
                                             (node 9 nil nil)))))
             && ((delete 10 
                 (node 10 (node 4 nil
                                 (node 6 (node 5 nil nil)
                                         (node 8 (node 7 nil nil)
                                                 (node 9 nil nil))))
                          (node 12 nil nil))) ==
                 (node 9 (node 4 nil
                                 (node 6 (node 5 nil nil)
                                         (node 8 (node 7 nil nil)
                                                 nil)))
                         (node 12 nil nil)))
             && ((delete 10
                    (node 10 nil
                            (node 14 (node 12 (node 11 nil nil)
                                              (node 13 nil nil))
                                     (node 15 nil nil)))) ==
                 (node 14 (node 12 (node 11 nil nil)
                                   (node 13 nil nil))
                          (node 15 nil nil)))
             && ((delete 2 (node 4 (node 2 (node 1 nil nil)
                                           (node 3 nil nil))
                                   (node 6 nil nil))) ==
                 (node 4 (node 1 nil (node 3 nil nil))
                         (node 6 nil nil)))
             && ((delete 4 (node 4 (node 2 (node 1 nil nil)
                                           (node 3 nil nil))
                                   (node 6 nil nil))) ==
                 (node 3 (node 2 (node 1 nil nil) nil)
                                 (node 6 nil nil)))
             && ((delete 7 
                 (node 5 nil
                         (node 10 (node 7 (node 6 nil nil)
                                          (node 8 nil
                                                 (node 9 nil nil)))
                                  nil))) ==
                 (node 5 nil
                         (node 10 (node 6 nil
                                          (node 8 nil
                                                  (node 9 nil nil)))
                                   nil)))
   -----------------------------------------------------
   -- top - функция, возвращающая число уровней в дереве
   -----------------------------------------------------
   testTop =   top nil              == -1
            && top (node 5 nil nil) == 0
            && top (node 5 (node 3 (node 1 nil nil)
                                   (node 4 nil nil))
                           (node 7 (node 6 nil nil) nil))
                                    == 2
            && top (node 8 (node 6 (node 1 nil nil) nil)
                           (node 10 nil
                                    (node 14 (node 12 nil nil)
                                             (node 22 nil nil))))
                                    == 3
            && top (node 7 (node 2 (node 3 (node 4 (node 5 nil nil)
                                                   nil)
                                              nil) nil) nil)
                                    == 4
   -----------------------------------------------------------
   -- topNode - функция, возвращающая уровень заданной вершины
   -----------------------------------------------------------
   testtopNode = topNode 15 (node 7 (node 6 nil nil)
                                    (node 9 (node 8 nil nil)
                                            (node 15 nil nil)))==2
              && topNode  9 (node 7 (node 6 nil nil)
                                    (node 9 (node 8 nil nil)
                                            (node 15 nil nil)))==1
   ---------------------------------------------------------------
   -- node - функция, возвращающая количество вершин в дереве
   ----------------------------------------------------------------
   testNode = nodes nil                                        == 0
           && nodes (node 5 nil nil)                           == 1
           && nodes (node 5 (node 3 (node 1 nil nil)
                                    (node 4 nil nil))
                            (node 7 (node 6 nil nil) nil))     == 6
           && nodes (node 8 (node 6 (node 1 nil nil) nil)
                            (node 10 nil
                                     (node 14 (node 12 nil nil)
                                              (node 22 nil nil))))
                                                               == 7
           && nodes (node 7 (node 2 (node 3 (node 4 (node 5 nil nil)
                                                    nil)
                                            nil) nil) nil)     == 5
   ----------------------------------------------------------------
   -- nList - функция, возвращающая количество листьев в дереве
   ------------------------------------------------------------
   testNList = ((nList nil) == 0)
            && ((nList (node 1 nil nil)) == 1)
            && ((nList (node 4 (node 2 nil (node 3 nil nil))
                               (node 6 nil nil))) == 2)
            && ((nList (node 8 (node 6 (node 1 nil nil) nil)
                               (node 10 nil
                                       (node 14 (node 12 nil nil)
                                                (node 22 nil nil))))
                ) == 3)
            && ((nList (node 5 (node 3 (node 1 nil nil)
                                       (node 4 nil nil))
                               (node 7 (node 6 nil nil)
                                       (node 13 nil nil)))) == 4)
   --------------------------------------------------------------
   -- Way - функция, возвращающая количество дуг в дереве
   ---------------------------------------------------------
   testWay = ((way (node 3 nil nil)) == 0)
          && ((way (node 3 (node 5 nil nil) (node 4 nil nil))) == 2)
          && ((way (node 4 (node 2 nil (node 3 nil nil))
                           (node 6 (node 5 nil nil)
                                   (node 7 nil nil)))) == 5)
          && ((way (node 10 (node 6 (node 3 nil nil)
                                    (node 7 nil (node 8 nil nil)))
                            (node 15 (node 12 nil nil)
                                     (node 18 (node 17 nil nil)
                                              nil)))) == 8)
   --------------------------------------------------------
   -- EqualTree - предикат, устанавливающий равенство двух
   --             бинарных деревьев поиска
   ---------------------------------------
   testEqualTree = equalTree nil nil
                && equalTree (node 3 nil nil)
                               (node 3 nil nil)
                && ((equalTree (node 3 (node 2 nil nil)
                                       (node 5 (node 4 nil nil)
                                               nil))
                               (node 3 (node 2 nil nil)
                                       (node 5 (node 4 nil nil)
                                               nil))) == True)
                && ((equalTree (node 3 (node 2 nil nil)
                                       (node 5 nil nil))
                               (node 3 (node 2 nil nil) nil))
                    == False)
                && ((equalTree
                    (node 4 (node 2 nil (node 3 nil nil))
                            (node 6 (node 5 nil nil)
                                    (node 7 nil nil)))
                    (node 4 (node 1 nil (node 3 nil nil))
                            (node 6 (node 5 nil nil)
                                    (node 8 nil nil)))) == False)
   --------------------------------------------------------------
   -- tCopy - функция, возвращающая копию бинарного дерева поиска
   --------------------------------------------------------------
   testTCopy =   tCopy nil == nil
              && tCopy (node 14 nil nil)
                           == node 14 nil nil
              && tCopy (node 3 (node 2 nil nil) (node 5 nil nil))
                           == node 3 (node 2 nil nil)
                                     (node 5 nil nil)
              && tCopy (node 4 (node 2 (node 1 nil nil)
                                       (node 3 nil nil))
                               (node 6 nil nil))
                           == node 4 (node 2 (node 1 nil nil)
                                             (node 3 nil nil))
                                     (node 6 nil nil)
              && tCopy (node 4 (node 2 nil nil)
                               (node 56 (node 16
                                              (node 12 nil nil) nil)
                                        (node 78 nil nil)))
                           == node 4 (node 2 nil nil)
                                     (node 56 (node 16
                                                   (node 12 nil nil)
                                                   nil)
                                              (node 78 nil nil))
   --------------------------------------------------------------
   -- klpObh - функция, осуществляющая левосторонний (нисходящий)
   --          обход бинарного дерева поиска
   ---------------------------------------------
   testKLP_Obh =   klpObh nil              == []
                && klpObh (node 5 nil nil) == [5]
                && klpObh (node 3 (node 2 nil nil)
                                  (node 4 nil nil))
                                           == [3,2,4]
                && klpObh (node 4 (node 2 nil (node 3 nil nil))
                                  (node 6 (node 5 nil nil)
                                          (node 7 nil nil)))
                                           == [4,2,3,6,5,7]
                && klpObh (node 5 (node 3 (node 1 nil nil)
                                          (node 4 nil nil))
                                  (node 7 (node 6 nil nil)
                                          (node 13 (node 11 nil nil)
                                                   (node 15 nil nil)
                                          )))
                                           == [5,3,1,4,7,6,13,11,15]
   -----------------------------------------------------------------
   -- lpkObh - функция, осуществляющая концевой (восходящий) обход
   --          бинарного дерева поиска
   ---------------------------------------------
   testLPK_Obh =   lpkObh nil              == []
                && lpkObh (node 3 nil nil) == [3]
                && lpkObh (node 3 (node 2 nil nil) (node 4 nil nil))
                                           == [2,4,3]
                && lpkObh (node 4 (node 2 nil (node 3 nil nil))
                                  (node 6 (node 5 nil nil)
                                          (node 7 nil nil)))
                                           == [3,2,5,7,6,4]
                && lpkObh (node 5 (node 3 (node 1 nil nil)
                                          (node 4 nil nil))
                                  (node 7 (node 6 nil nil)
                                          (node 13 (node 11 nil nil)
                                                   (node 15 nil nil)
                                          )))
                                           == [1,4,3,6,11,15,13,7,5]
   -----------------------------------------------------------------
   -- lkpObh - функция, осуществляющая обратный (смешанный) обход
   --          бинарного дерева поиска
   -----------------------------------
   testLKP_Obh =   lkpObh nil == []
                && lkpObh (node 3 (list 2) (list 4))
                              == [2,3,4]
                && lkpObh (node 3 (list 2) (node 4 nil (list 5)))
                              == [2,3,4,5]   
                && lkpObh (node 4 (node 2 nil (list 3)) 
                                  (node 6 (list 5) (list 7)))
                              == [2,3,4,5,6,7]
                && lkpObh (node 5 (node 3 (list 1) (list 4)) 
                                  (node 7 (list 6) 
                                          (node 9 (list 8)
                                                  (list 15))))
                              == [1,3,4,5,6,7,8,9,15]
   --------------------------------------------------
   testmapBTree1 = mapBTree (^2) nil
   testmapBTree2 = mapBTree (^2) (node 3 (list 2) (list 4))
   testmapBTree3 = mapBTree (^2) (node 3 (list 2)
                                         (node 4 nil (list 5)))
   testmapBTree4 = mapBTree (^2) (node 4 (node 2 nil (list 3)) 
                                         (node 6 (list 5) (list 7)))
   testmapBTree5 = mapBTree (^2)
                            (node 5 (node 3 (list 1)
                                            (list 4))
                                    (node 7 (list 6)
                                            (node 9 (list 8)
                                                    (list 15))))
   -------------------------------------------------------------
   -- outTree - функция, изображающая бинарное дерево поиска
   --           на экране дисплея
   ------------------------------
   testOut1 = outTree (list 3)
   testOut2 = outTree (node 3 (list 2) (list 4))
   testOut3 = outTree (node 4 (node 2 nil (list 3)) 
                              (node 6 (list 5) (list 7)))
   testOut4 = outTree (node 5 (node 3 (list 1) (list 4))          
                              (node 7 (list 6) (node 9 (list 8)   
                                                       (list 15))))
   testOut5 = outTree (node 10 (node 6 (list 3)
                                       (node 7 nil (list 8)))
                               (node 15 (list 12) 
                                        (node 18 (list 17) nil)))
   --------------------------------------------------------------
   testOut6 = show (node 10 (node 6 (list 3)
                                    (node 7 nil (list 8)))
                            (node 15 (list 12) 
                                     (node 18 (list 17) nil)))
   testOut7 = outTree' (node 10 (node 6 (list 3)
                                        (node 7 nil (list 8)))
                                (node 15 (list 12) 
                                         (node 18 (list 17) nil)))
   ---------------------------------------------------------------
   tree = node 10 (node 6 (list 3) (node 7 nil (list 8)))
                  (node 15 (list 12) 
                           (node 18 (list 17) nil))
   tree1 = node 10 (node 4 nil (node 6 (list 5)
                                       (node 8 (list 7) (list 9))))
                   (list 12)
   tree2 = node 10 nil
                (node 14 (node 12 (list 11) (list 13))
                         (list 15))
   tree3 = node 4 (node 2 (list 1) (list 3)) (list 6)
   tree4 = node 4 (node 2 (list 1) (list 3)) (list 6)
   tree5 = node 5 nil 
                  (node 10 (node 7 (list 6) 
                                   (node 8 nil (list 9))) nil)
   -----------------------------------------------------------
   test1  = filterBT even tree
   test2  = filterBT (>=10) tree
   test3  = filterBT odd tree
   test4  = filterBT even (consTree [4,6,5,7,3,8,9,2])
   ---------------------------------------------------
   test5  = delete' 5 (list 5) == nil
   ----------------------------------
   test6  = outTree tree1
   test7  = outTree (delete' 10 tree1)
   -----------------------------------
   test8  = outTree tree2
   test9  = outTree (delete' 10 tree2)
   -----------------------------------
   test10 = outTree tree3
   test11 = outTree (delete' 2 tree3)
   ----------------------------------
   test12 = outTree tree4
   test13 = outTree (delete' 4 tree4)
   ----------------------------------
   test14 = outTree tree5
   test15 = outTree (delete' 7 tree5)
   ------------------------------------------------------
   -- Демонстрация конструирования "вложенных" (от корня)
   -- случайных деревьев
   -------------------------------------
   test16 = outTree (newRndTree 3 20 18)
   test17 = outTree (newRndTree 3 10 18)
