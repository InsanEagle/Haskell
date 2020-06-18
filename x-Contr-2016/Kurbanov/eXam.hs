  -------------------����� ��� ��������-----------------------
  -- �1.(+) ��⠭���� �᫮��� �����.
  --
     zamena :: (Integral a, Floating b) => [(a,b)] -> [(a,b)]
     zamena = map (\(k,a) -> (k, a + fromIntegral k))
  -------------------------------------------------------------
  -- ������� �ணࠬ��, �� �室� ���ண� �㤥� ��� �ᥫ:
  -- 楫��᫥����� � ����⢥�����. �� ��室� �㤥� ᯨ᮪ ���,
  -- ��� ���� ������� ��⠭���� ���������, � ���� ������⮬
  -- �㤥� ����� �㬬� ��ࢮ�� � ��ண�. 
  -- **********************************************************
  -- �2.(+) ������ ����� ������� ᯨ᪠ �������� �᫮ ࠧ.
  --
  --  f "abc" 3
  -- "aaabbbccc"
  -------------------------------------------------------------
     f2:: [a] -> Int -> [a]
     f2 [] n = []
     f2 (x:xs) n = replicate n x ++ f2 xs n
  ----------
     test2 = f2 [1,2,3]   2       == [1,1,2,2,3,3]
        &&   f2 [5,3,2,6] 3       == [5,5,5,3,3,3,2,2,2,6,6,6]
        &&   f2 [-3,0,1,-5,-12] 4 == [-3,-3,-3,-3,0,0,0,0,1,1,1,1,
                                      -5,-5,-5,-5,-12,-12,-12,-12] 
  -- **********************************************************
  -- �3. ������ ����� n-� ������� ᯨ᪠.
  --
  -- f "abcdefghik" 3
  -- "abdeghk"
  -------------------------------------------------------------
  --
  -------------------------------------------------------------
  -- �4.(+) �뤥��� ���ᯨ᮪ � n-�� �� k-� ����� [n;k) 
  --
  -- f "abcdefghik" 2 5
  -- "cde"
  -------------------------------------------------------------
     underlst n k lst = drop n (take k lst)
  ----------
     test4 = underlst 3 5 [1,2,3,4,5,6,7]            == 4,5
        &&   underlst 6 9 "abcdefghijklm"            == ghi
        &&   underlst 2 4 [12,12,12,-13,14,51,61,54] == 12,-13
  -- **********************************************************
  -- �5. ������ 横������ ���� ᯨ᪠ �����.
  -- 
  -- f "abcdefghik" (-2)
  -- "ikabcdefgh"
  ------------------------------------------------------------   