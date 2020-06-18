   -- ���������� �������� ��䬥��᪨� ��ࠦ����,
   -- �࠭����� � ����୮� ��ॢ�-��㫥
   -- [��ࣨ��᪨�,2010,�.165-166] 
   -- ****************************
   module ArExpr where 
   data Expr = Const Int | Id String | App Op Expr Expr
   data Op   = Add | Sub | Mul | Div

   -- *******************************************
   -- �㭪�� �����頥� ���祭�� ��䬥��᪮��
   -- ��ࠦ���� � ���⥪�� env
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
   -- ���㠫����� ��䬥��᪨� ��ࠦ����
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
   -- ��㤠�� ��⮢� �ਬ���:
   --------------------------------------------
   test1 = App Sub (App Mul (Const 4) (Id "y"))
                   (App Add (Const 3) (Id "x"))
   --------------------------------------------------
   test2 = eval (App Sub (App Mul (Const 4) (Id "y"))
                         (App Add (Const 3) (Id "x"))
                )
                [("x",4),("y",2)] == 1
