   -- Демонстрация интерпретатора арифметических выражений,
   -- хранящихся в бинарном дереве-формуле
   -- [Сергиевский,2010,с.165-166] 
   -- ****************************
   module ArExpr where 
   data Expr = Const Int | Id String | App Op Expr Expr
   data Op   = Add | Sub | Mul | Div

   -- *******************************************
   -- Функция возвращает значение арифметического
   -- выражения в контексте env
   ------------------------------------
   eval:: Expr -> [(String,Int)] -> Int
   eval (Const x)    _  = x
   eval (Id str)    env = getval str env
   eval (App o x y) env = apply o (eval x env) (eval y env)

   -- ****************************
   apply:: Op -> Int -> Int -> Int
   apply Add x y = x + y
   apply Sub x y = x - y
   apply Mul x y = x * y
   apply Div x y = div x y

   -- *********************************************
   getval _id ((id,value) : xs) | (_id==id) = value
                                | True      = getval _id xs

   -- *************************************
   -- Визуализатор арифметических выражений
   ----------------------------------------
   instance Show (Expr) where
      show (Const c)   = show c
      show (Id s)      = s
      show (App o x y) = "(" ++ show o ++ " " ++ 
                                show x ++ " " ++  show y ++ ")"
   ------------------------------------------------------------
   instance Show (Op) where
      show (Add) = "+"
      show (Sub) = "-"
      show (Mul) = "*"
      show (Div) = "/"

   -- ***************************
   -- Неудачные тестовые примеры:
   --------------------------------------------
   test1 = App Sub (App Mul (Const 4) (Id "y"))
                   (App Add (Const 3) (Id "x"))
   --------------------------------------------------
   test2 = eval (App Sub (App Mul (Const 4) (Id "y"))
                         (App Add (Const 3) (Id "x"))
                )
                [("x",4),("y",2)] == 1
