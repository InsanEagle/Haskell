   -- ���������� ࠡ��� � "��栬�" � "�뭮��ﬨ"
   -- � ������� ��ॢ��� ���᪠
   -- **************************
   import Tree
   -----------------------------------------------------------
   -- �㭪�� �����頥� "���" ��� ��������  ���設� a �᫮-
   -- ���� ����୮�� ��ॢ� ���᪠ tree;
   -- fathers - ����������騩 ��ࠬ���
   --           (�� �맮�� fathers=[]),
   -- ����� ᮤ�ন� ᯨ᮪ "��殢", ����஥���  � �����
   -- ���᪠ ���設� a � ����୮� ��ॢ� ���᪠ tree
   -------------------------------------------------
   srchFath:: Int -> BTree -> [Int] -> Int
   srchFath a tree fathers 
              | isEmpty tree = error "��ॢ� ����"
              | a==root tree && null fathers
                             = error "��� � ���設� ���"
              | a==root tree = head fathers
              | a<root tree  = srchFath a 
                                        (left tree) 
                                        (root tree : fathers)
              | True         = srchFath a
                                        (right tree) 
                                        (root tree : fathers)
   ----------------------------------------------------------
   -- �㭪��, ��������� �� �������� ���設� x ᯨ᮪, �-
   -- ��ঠ騩  ��  "�뭮���", �  ����୮� ��ॢ� ���᪠ tree
   -- (0 ������砥� ��७� ���⮣� ��ॢ�)
   ---------------------------------------
   srchSon:: Int -> BTree -> [Int]
   srchSon x tree = sons (search x tree)
     where sons tree | isEmpty tree = []
                     | isEmpty (left tree) && isEmpty (right tree)
                                    = [0,0]
                     | isNode (left tree) && isEmpty (right tree)
                                    = [root (left tree),0]
                     | isEmpty (left tree) && isNode (right tree)
                                    = [0,root (right tree)]
                     | True         = [root (left tree),
                                       root (right tree)]
   ------------------------------------------------------
   -- �㭪��, ��������� �� ��������� ����୮�� ��ॢ�
   -- ���᪠ ᯨ᮪ ��� ��� ���
   -------------------------------
   edgeTree:: BTree -> [[Int]]
   edgeTree tree = filter (not . elem 0) (edgeTree' tree)
     where edgeTree' tree 
             | isEmpty tree = []
             | True         = [[root tree,
                                  head (srchSon (root tree) tree)]]
                             ++ [[root tree,
                                 last (srchSon (root tree) tree)]]
                             ++ edgeTree' (left tree)
                             ++ edgeTree' (right tree)
   --------------------------------------------------------------
   -- �।����, ��⠭�������騩, �� ����୮� ��ॢ� ���᪠ tree1
   -- ���� �����ॢ�� ����୮�� ��ॢ� ���᪠ tree2
   ----------------------------------------------------
   intT1T2:: BTree -> BTree -> Bool
   intT1T2 tree1 tree2 = intT1T2' (edgeTree tree1) (edgeTree tree2)
     where intT1T2' lst1 lst2
              | null lst1             = True
              | elem (head lst1) lst2 = intT1T2' (tail lst1) lst2
              | True                  = False
   ------------------------------------------------------
   -- �㭪��, ��������� �� ��������� ����୮�� ��ॢ�
   -- ���᪠ tree ᯨ᮪ �஥� ����:
   --
   --  [
   --   [����_1, ����_����_������_�����ॢ�_1, 
   --            ����_����_�ࠢ���_�����ॢ�_1]
   --   [����_2, ����_����_������_�����ॢ�_2, 
   --            ����_����_�ࠢ���_�����ॢ�_2]
   --                        ...
   --   [����_N, ����_����_������_�����ॢ�_N, 
   --            ����_����_�ࠢ���_�����ॢ�_N]
   --  ],
   --
   -- ��� ���� 㧫�� ���������� � ���浪� ��室� ��ॢ� ��
   -- �஢�� � ᮤ�ঠ��� � ᯨ᪥ lst
   ------------------------------------
   trojki:: BTree -> [Int] -> [[Int]]
   trojki tree lst 
         | lst==[] = []
         | True    = ([head lst] ++ srchSon (head lst) tree)
                     : trojki tree (tail lst)
   -- ***************************************
   -- ��㤠�� ��⮢� �ਬ���:
   -------------------------------------
   test1 =   srchFath 3 (node 4 (list 3) 
                                (list 5)) []                  == 4
          && srchFath 5 (Node 4 (Node 3 Nil Nil)
                                (Node 5 Nil Nil)) []          == 4
          && srchFath 5 (Node 4 (Node 2 Nil (Node 3 Nil Nil))
                                (Node 6 (Node 5 Nil Nil)
                                        (Node 7 Nil Nil))) [] == 6
          && srchFath 6 (Node 4 (Node 2 Nil (Node 3 Nil Nil))
                                (Node 6 (Node 5 Nil Nil)
                                        (Node 7 Nil Nil))) [] == 4
          && srchFath 17 (Node 10 (Node 6 (Node 3 Nil Nil)
                                          (Node 7 Nil
                                                  (Node 8 Nil Nil)))
                                  (Node 15 (Node 12 Nil Nil)
                                           (Node 18 (Node 17 Nil Nil)
                                                    Nil))) [] == 18
          && srchFath 7 (Node 10 (Node 6 (Node 3 Nil Nil)
                                         (Node 7 Nil
                                                 (Node 8 Nil Nil)))
                                 (Node 15 (Node 12 Nil Nil)
                                          (Node 18 (Node 17 Nil Nil)
                                                   Nil))) []  == 6
   ---------------------------------------------------------------
   -- ��ਠ�� ��ଫ���� ��⮢:
   -----------------------------
   test2 = srchFath v1 tree1 []
      where v1    = 8
            tree1 = node 4 (list 3) (list 5)
   -----------------------------------------
   test3 = srchFath v2 tree2 []
      where v2 = 10
            tree2 = node 10 (node 6 (list 3)           
                                    (node 7 nil (list 8))) 
                            (node 15 (list 12)         
                                     (node 18 (list 17) nil)) 
   ----------------------------------------------------------
   tree = node 10 (node 6 (list 3)
                          (node 7 nil (list 8)))
                  (node 15 (list 12) 
                           (node 18 (list 17) nil))
   tree1 = newRndTree 60 80 80
   tree2 = node 10 (list 6)
                   (node 15 (list 12) 
                            (node 18 (list 17) nil))
   -------------------------------------------------
   test4 =(   show (srchSon 10 tree) ++ " "
           ++ show (srchSon  8 tree) ++ " "
           ++ show (srchSon 18 tree) ++ " "
           ++ show (srchSon  7 tree),
           tree
          )
   --------------------------------------------------------
   test5 = putStr(   show (trojki tree [10])        ++ "\n"
                  ++ show (trojki tree [6,15])      ++ "\n"
                  ++ show (trojki tree [3,7,12,18]) ++ "\n"
                  ++ show (trojki tree [8,17]))
   --------------------------------------------
   test6 = edgeTree tree
   test7 = do
             outTree tree1
             putStr ("\n"++show (edgeTree tree1))
   test8 = do
             outTree tree; putStr("\n\n") 
             outTree tree2
             putStr ("\n" ++ show (intT1T2 tree2 tree))
