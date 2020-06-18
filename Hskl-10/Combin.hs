   -- Демонстрация интерпретатора функций, представленных в
   -- бесточечной форме записи и располагаемых  в  бинарном
   -- дереве с параметрами.
   --
   -- Автор: Кудрявцева И.А. (30.04.2011)
   --------------------------------------
   module Combin
   where
   -------------------------------------
   -- Структура абстрактного типа данных
   -------------------------------------
   data Expr =  Const Int 
              | Id String
              | App1  F1
              | App2  F2
              | App1' F1
              | App2' F2
              | App3' F3
              | App Expr Expr
   --------------------------
   data F1 = Abs | Inc | I
   data F2 = Add | Sub | Mul | Div | K | W
   data F3 = B | C | S
   ------------------------------------
   eval:: Expr -> [(String,Int)] -> Int
   eval (Const c) _     = c
   eval (Id str) env    = getval str env
   eval (App t1 t2) env = (get1 t1 t2) env
   ---------------------------------------------------
   -- Подстановка значений вместо свободных переменных
   -- в комбинаторное выражение
   ----------------------------------------
   getval:: String -> [(String,Int)] -> Int
   getval _id ((id,value):xs) | (_id==id) = value
                              | True      = getval _id xs
   ------------------------------------------------------
   -- Интерпретатор одного уровня дерева
   --------------------------------------------
   get1:: Expr -> Expr -> [(String,Int)] -> Int
   get1 (App1 f1) x env  = apply1 f1 (eval x env) 
   get1 (App1' f1) x env = eval (apply1' f1 x) env
   get1 (App f g) x env  = (get2 f g x) env
   ----------------------------------------
   -- Интерпретатор двух уровней дерева
   ----------------------------------------------------
   get2:: Expr -> Expr -> Expr -> [(String,Int)] -> Int
   get2 (App2 f2) g2 h2 env  = apply2 f2 (eval g2 env) 
                                          (eval h2 env)
   get2 (App1' f1) g1 h1 env = eval (App (apply1' f1 g1) h1) env
   get2 (App2' f2) g2 h2 env = eval (apply2' f2 g2 h2) env
   get2 (App f g) x1 x2 env  = eval (get3 f g x1 x2) env
   -----------------------------------------------------
   -- Интерпретатор трёх уровней дерева
   -------------------------------------------
   get3:: Expr -> Expr -> Expr -> Expr -> Expr
   get3 (App1' f1) g1 h1 k1 = App (App (apply1' f1 g1) h1) k1
   get3 (App2' f2) g2 h2 k2 = App (apply2' f2 g2 h2) k2
   get3 (App3' f3) g3 h3 k3 = apply3' f3 g3 h3 k3
   get3 (App f g) x1 x2 x3  = App (get3 f g x1 x2) x3
   --------------------------------------------------
   -- Интерпретаторы операций
   --------------------------
   apply1 Abs x   = abs x
   apply1 Inc x   = (+) x 1
   apply2 Add x y = (+) x y 
   apply2 Sub x y = (-) x y 
   apply2 Mul x y = (*) x y 
   apply2 Div x y = div x y 
   ------------------------------
   -- Интерпретаторы комбинаторов
   ------------------------------
   apply1' I f     = f
   apply2' K f x   = f
   apply2' W f x   = App (App f x) x
   apply3' B f g x = App f (App g x)
   apply3' C f g x = App (App f x) g
   apply3' S f g x = App (App f x) (App g x)
   -----------------------------------------
   -- Визуализатор комбинаторного выражения
   ----------------------------------------
   instance Show (Expr) where
      show (Const c) | c<0  = "(" ++ show c ++ ")"
                     | True = show c
      show (Id x)      = x 
      show (App1  f1)  = show f1
      show (App2  f2)  = show f2
      show (App1' c1)  = show c1
      show (App2' c2)  = show c2
      show (App3' c3)  = show c3
      show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

   instance Show (F1) where
      show (Abs) = "abs"
      show (Inc) = "inc"
      show (I)   = "I"

   instance Show (F2) where
      show (Add) = "(+)"
      show (Sub) = "(-)"
      show (Mul) = "(*)"
      show (Div) = "div"
      show (K)   = "K"
      show (W)   = "W"

   instance Show (F3) where
      show (B) = "B"
      show (C) = "C"
      show (S) = "S"
   -- **************************
   -- Неудачные тестовые примеры
   -- *************************************
   -- Представление комбинаторных выражений
   ----------------------------------------
   -- f1 = (abs x)
   f1 = App (App1 Abs) (Id "x") 
   ----------------------------
   -- f2 = (((+) x) y)
   f2 = App (App (App2 Add) (Id "x")) (Id "y")
   -------------------------------------------
   -- f3 = (((B abs) inc) x)
   f3 = App (App (App (App3' B) (App1 Abs)) 
                 (App1 Inc)) 
            (Id "x")
   --------------------------------------
   -- f3' = (((C ((B (B abs)) (+))) 1) x)
   f3' = App (App (App (App3' C)
                       (App (App (App3' B)
                                 (App (App3' B)
                                      (App1 Abs)))
                            (App2 Add)))
                  (Const 1))
             (Id "x")
   ---------------------------------
   -- f4 = ((((B (B abs)) (+)) x) y)
   f4 = App (App (App (App (App3' B)
                           (App (App3' B) (App1 (Abs))))
                      (App2 (Add)))
                 (Id "x"))   
            (Id "y")
   -------------------------------------------------
   -- f5 = ((((C ((B (C ((B B) (-)))) (+))) x) y) z)
   f5 = App (App (App (App (App3' C) 
                           (App (App (App3' B) 
                                     (App (App3' C) 
                                          (App (App (App3' B) 
                                                    (App3' B)) 
                                               (App2 Sub)))) 
                                (App2 Add))) 
                      (Id "x")) 
                 (Id "y"))
            (Id "z")  
   -------------------------------------
   -- f6 = ((((C ((B B) (+))) abs) x) y)
   f6 = App (App (App (App (App3' C) 
                           (App (App (App3' B) 
                                     (App3' B))
                                (App2 Add))) 
                      (App1 Abs)) 
                 (Id "x"))
            (Id "y")
   --------------------------------------
   -- f6' = (((C ((B (C (+))) abs)) x) y)
   f6' = App (App (App (App3' C) 
                       (App (App (App3' B) 
                                 (App (App3' C) 
                                      (App2 Add))) 
                            (App1 Abs))) 
                   (Id "x")) (Id "y")
   ----------------------------------
   -- f7 = ((K x) y)
   f7 = App (App (App2' K) (Id "x")) (Id "y") 
   ------------------------------------------
   -- f7' = ((W y) x)
   f7' =  App (App (App2' W) (App2 Add)) (Id "x")
   ----------------------------------------------
   -- f8 = (((((S (K S)) K) Inc) Abs) x) 
   f8 = App (App (App (App (App (App3' S) 
                                (App (App2' K) (App3' S))) 
                           (App2' K)) 
                      (App1 Inc))
                 (App1 Abs))
            (Id "x")
   --------------------------
   -- f8' = (((B Inc) Abs) x)
   f8' = App (App (App (App3' B) (App1 Inc)) (App1 Abs)) (Id "x")
   -- ************************************************************
   -- Вычисление значений комбинаторных выражений в заданной среде
   ---------------------------------------------------------------
   test =   eval f1 [("x",10)] == abs 10 
         && eval f1 [("x",-4)] == abs (-4)
         -----------------------------------------
         && eval f2 [("x",15),("y",-5)] == 15+(-5)
         && eval f2 [("x",-3),("y",5)]  == (+) (-3) 5
         --------------------------------------------
         && eval f3 [("x",10)]      == abs (10+1)
         && eval f3 [("x",-4)]      == abs ((+) (-4) 1)
         && eval f3 [("x",-30)]     == eval f3' [("x",-30)]
         && (((.) abs) (+ 1)) (-30) == 
                                 flip ((.) ((.) (abs)) (+)) 1 (-30)
         ----------------------------------------------------------
         && eval f4 [("x",-31),("y",22)]   == abs ((-31)+22)
         && eval f4 [("x",-2),("y",-5)]    == abs ((+) (-2) (-5))
         && (((.) ((.) (abs))) (+)) (-1) 2 == abs ((+) (-1) 2)
         -------------------------------------------------------
         && eval f5 [("x",-9),("y",31),("z",9)]   == (-9)-(31+9)
         && eval f5 [("x",3),("y",4),("z",5)]     == (-) 3 ((+) 4 5)
         && (((flip (((.) (flip (((.) (.)) (-)))) (+))) 1) 2) 3
                                                  == (-) 1 ((+) 2 3)
         -----------------------------------------------------------
         && eval f6 [("x",3),("y",-4)]          == 3+abs (-4)
         && eval f6 [("x",8),("y",-1)]          == (+) 8 (abs (-1))
         && ((flip (((.) (.)) (+))) abs) 1 (-2) == (+) 1 (abs (-2))
         ----------------------------------------------------------
         && eval f6'[("x",3),("y",-4)] == 3+abs(-4)
         && eval f6'[("x",-3),("y",4)] == 
                                       eval f6 [("x",-3),("y",4)]
         && (flip (((.) (flip (+))) abs)) (-5) (-20) 
                                       == (+) (-5) (abs (-20))
         && (flip (((.) (flip (+))) abs)) (-5) (-20) 
                                       == 
                            ((flip (((.) (.)) (+))) abs) (-5) (-20)
         ----------------------------------------------------------
         && eval f7 [("x",10),("y",20)]  ==  10
         && eval f7 [("x",-10),("y",20)] == -10
         --------------------------------------
         && eval f7' [("x",-4)] == -8
         && eval f7' [("x",1)]  ==  2
         ----------------------------
         && eval f8 [("x",-5)] == 6
         && eval f8 [("x",-5)] == eval f8' [("x",-5)]
         && eval f8 [("x",-5)] == ((((.) (+ 1)) abs) (-5))
