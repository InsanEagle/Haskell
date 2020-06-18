   -- Демонстрация функции, преобразующей произвольный ламбда-
   -- терм в CL-терм в базисе {I,B,C,K,S} по правилам:
   --
   -- \x.x=I;
   --
   -- \x.PQ=K(PQ),         если x не входит свободно в лямбда-терм P
   --                      и x не входит свободно в лямбда-терм Q;
   --
   -- \x.PQ=BP(\x.Q),      если x не входит свободно в лямбда-терм P 
   --                      и x входит свободно в лямбда-терм Q;
   --
   -- \x.PQ=C(\x.P)Q,      если x входит свободно в лямбда-терм P и
   --                      x не входит свободно в лямбда-терм Q;
   --
   -- \x.PQ=S(\x.P)(\x.Q), если x входит свободно в лямбда-терм P и
   --                      x входит свободно в лямбда-терм P.
   --
   -- Автор: Душкин Р. [2007,с.487]
   -- *****************************
   module Transf_IBCS where
   import L1
   -- **********************
   transf:: Lambda -> Lambda
   transf (Var x) = Var x
   transf (App x y) = App (transf x) (transf y)
   transf (Lam x (Var y))
         | x==y = i
         | True = App k (Var y)
   transf (Lam x (App e1 e2))
         | free x e1 && free x e2 = 
                   App (App s (transf (Lam x e1)))
                       (transf (Lam x e2))
         | free x e1 && (not.free x) e2 =
                   App (App c (transf (Lam x e1))) (transf e2)
         | (not.free x) e1 && free x e2 = 
                   App (App b (transf e1)) (transf (Lam x e2))
         | True = App k (App e1 e2)
   transf (Lam x l@(Lam y e)) = transf (Lam x (transf l))

   -- *************************************************
   -- Предикат, устанавливающий, является ли предметная
   -- переменная x свободной в заданном ламбда-терме
   -------------------------------------------------
   free:: [Char] -> Lambda -> Bool
   free x (Var y)     = x==y
   free x (App e1 e2) = free x e1 || free x e2
   free x (Lam y e)   = x/=y && free x e

   -- *************************************
   -- Константные функции для представления
   -- базисных комбинаторов I, K, S, B, C
   --------------------------------------
   i = Var "I"
   k = Var "K"
   s = Var "S"
   b = Var "B"
   c = Var "C"

   -- ****************************************************
   -- Функция, представляющая комбинатор неподвижной точки
   -------------------------------------------------------
   y x = App x (y x)

   -- **************************************************
   -- Функции, представляющие CL-термы для тестирования:
   -----------------------------------------------------
   lam14 = Lam "x" (App (Var "x") 
                        (App (App (Var "y") (Var "x")) 
                             (Var "z")))
   ---------------------------------------------------
   lam24 = Lam "x" (Lam "y" (App (Var "x") (Var "x")))
   lam34 = Lam "x" (Lam "y" (App (Var "y") (Var "x")))
   lam44 = Lam "x" (Lam "y" (App (Var "x") (Var "y")))
   lam54 = Lam "f" (Lam "x" (App (App (Var "f") (Var "x")) 
                                 (Var "x")))
   lam64 = Lam "x" (App (Var "b") (App (Var "f") 
                                  (App (Var "a") (Var "x"))))
   ----------------------------------------------------------
   lam74 = Lam "x" (Lam "y" (Lam "z" 
                               (App (App (Var "x") (Var "z")) 
                                    (App (Var "y") (Var "z")))))
   -------------------------------------------------------------
   lam84 = Lam "x" (App (App (App s (Var "x")) (Var "y"))
                        (Var "z"))
   -----------------------------------------------
   lam94 = Lam "x" (App (App (Var "x") (Var "z")) 
                        (App (Var "y") (Var "z")))
   ----------------------------------------------------
   lam104 = Lam "x" (Lam "y" (App (Var "I") (Var "x")))
   lam114 = Lam "x" (Lam "y" (App (Var "y") (Var "x")))
   ---------------------------------------------------------
   lam124 = Lam "f" (Lam "x" (App (App (Var "f") (Var "x"))
                                  (Var "x")))
   ------------------------------------------
   lam134 = Lam "x" (Lam "y" (Lam "f"
                                 (App (App (Var "f") (Var "x"))
                                      (Var "y"))))
   --------------------------------------------------------
   lam135 = Lam "x" (Lam "y" (App (App (Var "M") (Var "x"))
                                  (App (Var "P") (Var "y"))))
   ----------------------------------------------------------
   lam136 = Lam "x" (Lam "y" (App (App (Var "M")
                                       (App (Var "P") (Var "x")))
                                  (App (Var "Q") (Var "y"))))
   ----------------------------------------------------------
   lam137 = Lam "f" (Lam "n" 
                      (App (App (Var "p") (Var "n"))
                           (App (Var "f") 
                                (App (Var "a") 
                                     (App (Var "1") (Var "n"))))))
   ---------------------------------------------------------------
   lam138 = Lam "f"
              (Lam "x"
               (App (App
                      (App (Var "cond") 
                           (App (App (Var "=") (Var "x")) 
                                (Var "1")))
                      (Var "1"))
                    (App 
                       (App (App (Var "*") (Var "x")) 
                            (Var "f"))
                       (App (App (Var "-") (Var "x")) (Var "1")))))
   ----------------------------------------------------------------
   lam139 = Lam "x" (Var "x")
   lam140 = Lam "x" (Lam "y" (Var "y"))
   -----------------------------------------------------
   -- Функция, моделирующая комбинатор неподвижной точки
   -- с вызовом по значению (по Д.Тромпу)
   --
   --  (\x.\y.xyx)(\y.\x.y(xyx))
   -----------------------------
   lam141 = App 
             (Lam "x" (Lam "y" (App (App (Var "x") (Var "y"))
                                         (Var "x"))))
             (Lam "y" (Lam "x" (App (Var "y") 
                                    (App (App (Var "x") (Var "y"))
                                         (Var "x")))))
   ---------------------------------------------------
   lam142 = Lam "f" (Lam "x" (App (Var "f")
                                  (App (Var "x") (Var "x"))))
   ----------------------------------------------------------
   lam143 = Lam "m" (Lam "n" (Lam "f" (Lam "x"
               (App (App (App (Var "n") (Var "m")) 
                         (Var "f")) 
                    (Var "x")))))
   ------------------------------
   lam144 = Lam "a" (Lam "b" 
                      (Lam "c" (Lam "d"
                                 (App (App (Var "a") (Var "b"))
                                      (App (Var "c") (Var "d"))))))
   ----------------------------------------------------------------
   lam145 = Lam "p" (Lam "z" 
               (App (App (Var "z")
                         (App (Var "p") k))
                    (App (Var "p")
                         (App k i))))
   ----------------------------------
   lam146 = Lam "b" (Lam "x" (Lam "y" 
                 (App (App (Var "b") (Var "x")) (Var "y"))))
   ---------------------------------------------------------
   lam147 = Lam "x" (Lam "y" 
                    (App (App (Var "x")
                              (Lam "u" (Lam "v" (Var "u"))))
                         (Var "y")))
   ----------------------------------------------------
   lam148 = Lam "x" (Lam "y" (Lam "z" (Lam "u" (Lam "v"
                      (App (Var "z") (App (Var "y") (Var "v")))
                    ))))
   ----------------------------------------------------
   lam149 = Lam "x" (Lam "y" (Lam "z" (Lam "u" (Lam "v"
                      (App (Var "z")
                           (App (App (Var "x") (Var "u"))
                                (Var "u")
                           )
                      )
                    ))))
   ----------------------------------
   lam150 = Lam "g" (Lam "f" (Lam "x"
                                 (App (Var "g")
                                      (App (Var "f") (Var "x"))
                                 )))
   -- ******************************
   -- Неудачные тестовые примеры:
   ------------------------------
   test1  = transf lam14
   test2  = show (transf lam24) =="BK(SII)"
   test3  = transf lam34
   test4  = transf lam44
   test5  = transf lam54
   test6  = transf lam64
   test7  = transf lam74
   test8  = transf lam84
   test9  = show (transf lam94) =="C(CIz)(yz)"
   test10 = show (transf lam104)=="BK(BII)"
   test11 = transf lam114
   test12 = transf lam124
   test13 = transf lam134
   test14 = transf lam135
   test15 = transf lam136
   test16 = transf lam137
   test17 = transf lam138
   test18 = transf lam139
   test19 = transf lam140
   test20 = transf lam141
   test21 = transf lam142
   test22 = transf lam143
   test23 = transf lam144
   test24 = transf lam145
   test25 = transf lam146
   test26 = transf lam147
   ----------------------
   test27 = transf lam148
   test28 = transf lam149
   test29 = transf lam150