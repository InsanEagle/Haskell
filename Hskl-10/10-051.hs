   -- ��।����, ����� ����� ������ � ������� ��-
   -- ������ ���� �㭪権.
   -- ��������. ���ᬮ��� ��䬥��᪮� ��ࠦ����
   --           (a+b)*b
   -- ********************************
   -- ������஢���� ��������஢ W � S
   -----------------------------------
   s a b c = a c (b c)
   -------------------
   w a b   = a b b
   -- **************
   -- ��稥 �㭪樨
   -------------------------------
   tst1 = (.) w ((.) ((.)(*)) (+))
   ---------------------------------------
   tst2 x = (.) ((.)((.) w) flip) ((.)(.)) 
                   (\x -> \y -> x*y) (\x -> x+1) x
              == s (\x -> \y -> x*y) (\x -> x+1) x
   ---------------------------------------------------------------
   tst3 = w (flip ((.) (.) ((.) (==) (\x -> x+1))) (\x -> x+1)) 23
