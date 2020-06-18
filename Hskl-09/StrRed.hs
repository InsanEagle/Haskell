   -- ���������� ��ப���� ।�樨 ��������୮�� ��ࠦ����.
   -- �ணࠬ�� ࠡ�⠥� � �������� ghci.
   --
   -- ����: ��᪢�� �.�. (20.02.2007)
   -- ********************************
   module StrRed
   where
   ---------------------------------------
   -- ������� �����ࠨ�᪮�� ⨯� ������
   ---------------------------------------
   data Cmb =  Cmb String   -- ���������
             | (:#) Cmb Cmb -- ��������� ��������஢ - ���������
                            -- (��䨪�� ���������)
      deriving (Eq)
   -----------------------
   instance Show Cmb where
      show (Cmb x)   = x
      show (ts :# t) = case t of _ :# _ -> show ts ++ 
                                               "(" ++ show t ++ ")"
                                 _      -> show ts ++ show t
   -------------------------------------------------------------
   -- �।�⠢����� ��䨪᭮��  ��������� �ਬ������ (:#) ��-
   -- �����樠⨢�� � �ਮ��⮬ 5,  �⮡� ������ ���������-
   -- ���� ��ࠦ���� �뫠 ��� ��譨� ᪮���. ���ਬ��, ��ࠦ���� 
   -- (SK)K==SKK
   -- �㤥� �।�⠢��� ��ப��
   -- s:#k:#k
   -----------------------------------------------
   -- ����砭��. �������� ������ instance Read Cmb
   -----------------------------------------------
   infixl 5 :#
   -----------------------
   -- �᭮��� ����������
   -------------------------
   b = Cmb "B" -- ���������
   w = Cmb "W" -- �㡫�����
   k = Cmb "K" -- ���楫���
   i = Cmb "I" -- ������쭨�
   s = Cmb "S" -- ��������
   c = Cmb "C" -- �������
   ----------------------------------
   -- ��ᯥ���஢���� ����������
   ----------------------------------
   x = Cmb "x"
   y = Cmb "y"
   z = Cmb "z"
   t = Cmb "t"
   u = Cmb "u"
   v = Cmb "v"
   --------------------------------------------------
   -- �㭪�� ��蠣����� �뢮�� १���� �ਬ������ 
   -- ��������஢ � ��������୮� ��ࠦ����
   -----------------------------------------
   modN:: Cmb -> String
   modN cmb = let res = mod1 cmb 
              in if res == cmb then (show cmb) 
                               else ((show cmb) ++ " -> " 
                                                ++ modN res)
   ---------------------------------------------------------
   -- �㭪�� �ਬ������  ᠬ���  ������ ��������� � 
   -- ��������୮� ��ࠦ����, ����� ����� �ਬ�����
   ---------------------------------------------------
   mod1:: Cmb -> Cmb
   mod1 (Cmb c) = (Cmb c)
   ---------------------------
   -- �ਬ������ ��������� I
   ---------------------------
   mod1 ((Cmb c) :# t) 
             | c == "I"   = t
             | True       = (Cmb c) :# mod1 t
   ------------------------------------------
   -- �ਬ������ ��������஢ W, K
   -------------------------------
   mod1 ((Cmb c) :# t :# s)
             | c == "W"   = t :# s :# s
             | c == "K"   = t
             | True  = mod1' ((Cmb c) :# t) s
   ------------------------------------------
   -- �ਬ������ ��������஢ B, S, C
   ----------------------------------
   mod1 ((Cmb c) :#t :# s :# u)
             | c == "B"  = t :# (s :# u)
             | c == "S"  = t :# u :# (s :# u)
             | c == "C"  = t :# u :# s
             | True  = mod1' ((Cmb c) :# t :# s) u
   -----------------------------------------------
   mod1 (ts :#t :# s :# u) = mod1' (ts :#t :# s) u
   ------------------------------------------------
   -- �㭪��, ������ � ��������୮� ��ࠦ���� 
   -- �ਬ����� ��������� � �ਬ������ ���
   ------------------------------------------
   mod1' :: Cmb -> Cmb -> Cmb
   mod1' c1 c2 = let res = mod1 c1
                 in if res == c1 then c1 :# mod1 c2 
                                 else res :# c2
   --------------------------------------------
   -- ����� ��������஢:
   ----------------------
   -- B � ����� SK
   -----------------------------
   b_sk = s:#(k:#s):#k :#x:#y:#z
   -----------------------------
   -- W � ����� SK
   -------------------------------------
   w_sk = s:#s:#(k:#(s:#k:#k)) :#x:#y:#z
   -------------------------------------
   -- C � ����� SK
   ---------------------------------------------------------------
   c_sk = s:#((s:#(k:#s):#k):#(s:#(k:#s):#k):#s):#(k:#k) :#x:#y:#z
   ---------------------------------------------------------------
   -- I � ����� SK
   ------------------
   i_sk = s:#k:#k :#x
   -----------------------------------------
   -- ��������� ����������� �窨 Y (����)
   -----------------------------------------
   yCurry = s:#(b:#w:#b):#(b:#w:#b) :#x
   ------------------------------------------
   -- ��������� ����������� �窨 Y (�஬��)
   ---------------------------------------------------------
   yTromp = s:#s:#k:#(s:#(k:#(s:#s:#(s:#(s:#s:#k)))):#k) :#x
   ---------------------------------------------------------
   -- ���������, �� ����騩 ��ଠ�쭮� ���
   ------------------------------------------
   noNF = s:#i:#i:#(s:#i:#i) :#x:#y

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test1 = modN b_sk ==
           "S(KS)Kxyz -> KSx(Kx)yz -> S(Kx)yz -> Kxz(yz) -> x(yz)"
   test2 = modN (c:#i:#x:#(s:#y:#i))
   test3 = take 200 $ modN yCurry
   -- "S(BWB)(BWB)x -> BWBx(BWBx) -> W(Bx)(BWBx) -> 
   -- Bx(BWBx)(BWBx) -> x(BWBx(BWBx)) -> x(W(Bx)(BWBx)) -> 
   -- x(Bx(BWBx)(BWBx)) -> x(x(BWBx(BWBx))) -> x(x(W(Bx)(BWBx))) ->
   -- x(x(Bx(BWBx)(BWBx))) -> x(x(x(BWBx(BWBx)))) -> {Interrupted!}"
   -----------------------------------------------------------------
   test4 = modN (u:#(c:#i:#x):#v:#y)
   test5 = take 200 $ modN yTromp
   test6 = modN (c:#s:#i:#x:#y:#z)
   test7 = modN (s:#k:#k:#x)
   test8 = modN (s :# (k :# s) :# k :# x :# y :# z)
   test9 = modN b_sk
   --------------------------------
   test10 = modN (c :# x :# y :# z)
   test11 = modN (s :# ((s :# (k :# s) :# k) :# 
                 (s :# (k :# s) :# k) :# s) :# 
                 (k :# k):# x :# y :# z)   
                              -- C = S((S(KS)K)(S(KS)K)S)(KK)
   test12 = (==) (z test8) (z test9)
        where z x = reverse $ takeWhile (/='>') $ reverse x
   --------------------------------------------------------