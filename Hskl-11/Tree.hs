   -- �����, ����뢠�騩 �����ࠨ�᪨� ⨯ ������ (���),
   -- ���������騩 �᫮�� ������ ��ॢ�� ���᪠, �।-
   -- �⠢����� ��������:
   --
   --               �����祭��� ��ꥤ������
   --                   |
   --  data BTree = Nil | Node Int (BTree) (BTree)
   --                          -------------------
   --                                   |
   --                         �����⮢� �ந��������
   --
   -- �����: �.�.����楢�, �.�.���檨� (15.12.2010)
   ---------------------------------------------------
   module Tree
      (BTree (Nil,Node),
      ---------------------------------------------------------
      -- ��� �।�⠢����� "�����饣�" ��� ����室���  㤠����
      -- ("������") ���祭� ��������஢ ������ (Nil,Node),
      -- �.�. ������� "(BTree (..),"
      -----------------------------------------------------
       nil, list, node,                     -- ����������
       root, left, right,                   -- ��������
       isEmpty, isNode,                     -- �।�����
       -------------------------------------------------
       addTree, consTree, consTree', consTree'',
       search, searchBool,
       rightList, leftList, delete, ud, delete', ud',
       top, topNode, nodes, nList, way, equalTree, tCopy,
       klpObh, lpkObh, lkpObh, klkpObh,
       newRndTree,
       sort,
       mapBTree, filterBT,                  -- �㭪樮���� 
       drawTree, outTree, outTree',         -- "���㠫������"
       ---------------------------------------------------------
       -- �㭪樨, �� 㤮���⢮���騥 �ॡ������, �।�塞�
       -- � ���
       ----------
       mapBTree')
   where
   -------------------
   -- ��������� ⨯�
   --     |
   --     |   ���������� ������
   --     |      |     |
   --     |      |     |
   data BTree = Nil | Node Int BTree BTree
   --              -       ---------------
   --              |              |
   --           �����⮢� �ந��������
   -----------------------------------
      deriving (Eq)
   --------------------------------------------
   -- ���������� ������ ����� ᫥���騥 ⨯�:
   -- > :t Nil
   -- BTree
   -- > :t Node
   -- Int -> BTree -> BTree -> BTree

   -- **************************************
   -- �㭪樨-���������� ������ ⨯� BTree
   -- (���������� - �� ������������ �㭪権, ᮧ�����
   -- �����⮢� �ந��������)
   -- ***********************

   ------------------------------------------------------
   -- �㭪��-��������� ���⮣� ����୮�� ��ॢ� ���᪠
   ------------------------------------------------------
   nil:: BTree
   nil = Nil
   ----------------------------------------------------
   -- �㭪��-��������� ���� ����୮�� ��ॢ� ���᪠
   ----------------------------------------------------
   list:: Int -> BTree
   list x = Node x Nil Nil
   -------------------------------------------------------
   -- �㭪��-��������� ���設� ����୮�� ��ॢ� ���᪠,
   -- �� ��饩�� ���⮬
   -------------------------------------
   node:: Int -> BTree -> BTree -> BTree
   node x l r = Node x l r

   -- ***********************************
   -- �㭪樨-ᥫ����� ������ ⨯� BTree
   -- (ᥫ����� - �� �㭪樨, ���ᯥ稢��騥 ����祭��
   -- �⤥���� ��������⮢ �����⮢�� �ந��������)
   -- **********************************************

   ------------------------------------------------------------
   -- �㭪��-ᥫ����, ��������� ��७� ��ॢ� ���᪠ BTree
   ------------------------------------------------------------
   root:: BTree -> Int
   root Nil          = error "��ॢ� ����"
   root (Node a l r) = a
   --------------------------------------------------------
   -- �㭪��-ᥫ����, ��������� ����� �����ॢ� ��ॢ�
   -- ���᪠ BTree
   ---------------------
   left:: BTree -> BTree
   left Nil          = nil
   left (Node a l r) = l
   ---------------------------------------------------------
   -- �㭪��-ᥫ����, ��������� �ࠢ�� �����ॢ� ��ॢ�
   -- ���᪠ BTree
   ----------------------
   right:: BTree -> BTree
   right Nil          = nil
   right (Node a l r) = r

   -- *************************************************
   -- �।����� ��� �ᯮ��������  ��������஢ ������
   -- ⨯� BTree ��� ������⮢ ࠧ��祭���� ��ꥤ������
   -- (�।����� - �� �㭪樨, ��������騥 �������-
   -- ஢��� �ਭ���������� ��������� ���祭�� ������-
   -- ���� �������� �� ࠧ��祭���� ��ꥤ������)
   -- *******************************************

   -------------------------------------------------
   -- �㭪��-�।����, �ᯮ������ ��������� Nil
   -------------------------------------------------
   isEmpty:: BTree -> Bool 
   isEmpty Nil = True
   isEmpty _   = False
   --------------------------------------------------
   -- �㭪��-�।����, �ᯮ������ ��������� Node
   --------------------------------------------------
   isNode:: BTree -> Bool
   isNode = not . isEmpty

   -----------------------------------------------------
   -- �஢�ਬ ⥪⮭�筮��� (����७��� ᮣ��ᮢ�������
   -- ��������) ����஥����� �����ࠨ�᪮�� ⨯� ������
   ------------------------------------------------------
   test0 = node (root t) (left t) (right t)
        where t = node 1 nil nil

   -- ***************************************************
   -- ��������� ����権 ��� �����ࠨ�᪨� ⨯�� ������
   -- ***************************************************

   -------------------------------------------------------
   -- �㭪��, ���������� ������� x � ��ॢ� ���᪠ BTree
   -------------------------------------------------------
   addTree:: Int -> BTree -> BTree
   addTree x tree | isEmpty tree = list x
                  | x==root tree = tree
                  | x<root tree  = node (root tree)
                                        (addTree x (left tree))
                                        (right tree)
                  | True         = node (root tree) 
                                        (left tree)
                                        (addTree x (right tree))

   -----------------------------------------------------------
   -- �㭪��-�����, ������������ ����୮� ��ॢ� ���᪠ ��
   -- �����஢������ ᯨ᪠ lst, �������� ���ண� "����㯠��"
   -- � ��ॢ� � ���浪� �� ᫥������� � ᯨ᪥
   --------------------------------------------
   consTree:: [Int] -> BTree
   consTree lst | null lst = nil
                | True     = addTree (last lst) 
                                     (consTree (init lst))

   -----------------------------------------------------------
   -- �㭪��-ࠧ���⪠, ������������ ����୮� ��ॢ� ���᪠
   -- �� �����஢������ ᯨ᪠, ��������  ���ண� "����㯠��"
   -- � ��ॢ� � ���浪�, ���⭮� �� ᫥������� � ᯨ᪥
   ------------------------------------------------------
   consTree':: [Int] -> BTree
   consTree' = foldr addTree nil

   -----------------------------------------------------------
   -- �㭪��-ࠧ���⪠, ������������ ����୮� ��ॢ� ���᪠
   -- �� �����஢������ ᯨ᪠ lst, �������� ���ண� "����㯠-
   -- ��" � ��ॢ� � ���浪� �� ᫥������� � ᯨ᪥
   ------------------------------------------------
   consTree'':: [Int] -> BTree
   consTree'' = foldl (flip addTree) nil

   ------------------------------------------------------------
   -- �㭪��, �����⢫���� ������ ���� ������� a � ���-
   -- �� ���᪠ tree:
   --  (1) � ��砥 �ᯥ� �����頥� �����ॢ� ��ॢ�  Tree, �
   -- ���஬ ������� a ���� ��୥�;
   --  (2) � ��砥 ��㤠筮�� ���᪠ �㭪�� �����頥� ���⮥
   -- ��ॢ� nil
   ------------------------------
   search:: Int -> BTree -> BTree
   search a tree | isEmpty tree = nil
                 | a==root tree = tree
                 | a<root tree  = search a (left tree) 
                 | True         = search a (right tree)

   -----------------------------------------------------------
   -- �㭪��-�।����, �����⢫���� ������ ���� �������
   -- a � ��ॢ� ���᪠:
   --  (1) � ��砥 �ᯥ� �����頥� True;
   --  (2) � ��砥 ��㤠筮�� ���᪠ - False
   ------------------------------------------
   searchBool:: Int -> BTree -> Bool
   searchBool a tree | isEmpty tree = False
                     | a==root tree = True
                     | a<root tree  = searchBool a (left tree)
                     | True         = searchBool a (right tree)

   -------------------------------------------------------------
   -- �㭪��, ��������� ᠬ� �ࠢ� ���� ��ॢ� ���᪠ tree
   -------------------------------------------------------------
   rightList:: BTree -> Int
   rightList tree | isEmpty (left tree) && isEmpty (right tree)
                                         = root tree
                  | isEmpty (right tree) = rightList (left tree)
                  | True                 = rightList (right tree)

   ------------------------------------------------------------
   -- �㭪��, ��������� ᠬ� ���� ���� ��ॢ� ���᪠ tree
   ------------------------------------------------------------
   leftList:: BTree -> Int
   leftList tree | isEmpty (left tree) && isEmpty (right tree)
                                       = root tree
                 | isEmpty (left tree) = leftList (right tree)
                 | True                = leftList (left tree)

   -------------------------------------------------------------
   -- �㭪��, 㤠����� 㧥� x �� ����୮�� ��ॢ� ���᪠ tree.
   -- �㭪樨 delete() � ud() "��᫮���"  �������� ᮮ⢥����-
   -- �騥 ४��ᨢ�� ��楤��� �.���� [1985] (�� Pascal)
   -- (���� ᯮᮡ)
   ------------------------------
   delete:: Int -> BTree -> BTree
   delete x tree | isEmpty tree = nil
                 | x<root tree  = node (root tree) 
                                       (delete x (left tree))
                                       (right tree)
                 | x>root tree  = node (root tree) 
                                       (left tree)
                                       (delete x (right tree))
                 | isEmpty (right tree)
                                = left tree
                 | isEmpty (left tree)
                                = right tree
                 | True         = node (ud (left tree))
                                       (delete (ud (left tree))
                                               (left tree))
                                       (right tree)
   --------------------------------------------------------
   -- �㭪��, ��������� ᠬ� "�ࠢ�" ������� (�������
   -- � �������訬 ���箬) ����୮�� ��ॢ� ���᪠ tree
   ----------------------------------------------------
   ud:: BTree -> Int
   ud tree | isEmpty (right tree) = root tree
           | True                 = ud (right tree)

   --------------------------------------------------------
   -- �㭪��, 㤠����� 㧥� x �� ����୮�� ��ॢ� ���᪠.
   -- �㭪樨 delete'()  �  ud'() �������� ᮮ⢥�����騥
   -- ४��ᨢ�� ��楤��� �.���� [1985] (�� Pascal)
   -- (��ன ᯮᮡ)
   -------------------------------
   delete':: Int -> BTree -> BTree
   delete' x tree 
              | isEmpty tree = nil
              | x<root tree  = node (root tree) 
                                    (delete' x (left tree))
                                    (right tree)
              | x>root tree  = node (root tree) 
                                    (left tree)
                                    (delete' x (right tree))
              | isEmpty (right tree)
                             = left tree
              | isEmpty (left tree)
                             = right tree
              | True         = node (ud' (right tree))
                                    (left tree)
                                    (delete' (ud' (right tree))
                                             (right tree))
   -------------------------------------------------------
   -- �㭪��, ��������� ᠬ� "����" ������� (�������
   -- � �������訬 ���箬) ����୮�� ��ॢ� ���᪠ tree
   ----------------------------------------------------
   ud':: BTree -> Int
   ud' tree | isEmpty (left tree) = root tree
            | True                = ud' (left tree)

   ----------------------------------------------------------
   -- �㭪��, ��������� ������⢮ �஢��� � ����୮� ��-
   -- ॢ� ���᪠ tree (��७� ��ॢ� �ᯮ����� �� �஢�� 0)
   ----------------------------------------------------------
   top:: BTree -> Int
   top tree | isEmpty tree = -1
            | True         = 1+max (top (left tree))
                                   (top (right tree))

   ---------------------------------------------------
   -- �㭪��, ��������� �஢��� �������� ���設� x
   -- � ����୮� ��ॢ� ���᪠ tree
   --------------------------------
   topNode:: Int -> BTree -> Int
   topNode x tree | x==root tree = 0
                  | x<root tree  = 1 + topNode x (left tree)
                  | True         = 1 + topNode x (right tree)

   ------------------------------------------
   -- �㭪��, ��������� ������⢮ ���設
   -- � ����୮� ��ॢ� ���᪠ tree
   --------------------------------
   nodes:: BTree -> Int
   nodes tree | isEmpty tree = 0
              | True         = 1 + nodes (left tree)
                                 + nodes (right tree)

   -------------------------------------------
   -- �㭪��, ��������� ������⢮ ����쥢
   -- � ����୮� ��ॢ� ���᪠ tree
   --------------------------------
   nList:: BTree -> Int
   nList tree | isEmpty tree = 0
              | isEmpty (left tree) && isEmpty (right tree)
                             = 1
              | True         = nList (left tree) +
                               nList (right tree)

   --------------------------------------------------
   -- �㭪��, ��������� ������⢮ �� � ����୮�
   -- ��ॢ� ���᪠ tree
   ---------------------
   way:: BTree -> Int
   way tree | isEmpty (left tree) && isEmpty (right tree)
                                   = 0
            | isEmpty (left tree)  = 1 + way (right tree)
            | isEmpty (right tree) = 1 + way (left tree)
            | True                 = 2 + way (left tree)
                                       + way (right tree)

   ---------------------------------------------------
   -- �।����, ��⠭�������騩 ࠢ���⢮ �ந�������
   -- ������� ��ॢ쥢 ���᪠ tree1 � tree2
   -----------------------------------------
   equalTree:: BTree -> BTree -> Bool
   equalTree tree1 tree2 | isEmpty tree1 && isEmpty tree2
                                   = True
                         | isEmpty tree1 || isEmpty tree2
                                   = False
                         | True    =   root tree1==root tree2
                                    && equalTree (left tree1)
                                                 (left tree2)
                                    && equalTree (right tree1)
                                                 (right tree2)

   -----------------------------------------------------------
   -- �㭪��, ��������� ����� ����୮�� ��ॢ� ���᪠ tree
   -----------------------------------------------------------
   tCopy:: BTree -> BTree
   tCopy tree | isEmpty tree = nil
              | True         = node (root tree)
                                    (tCopy (left tree))
                                    (tCopy (right tree))

   -- ***************************
   -- ������ (��室� ��ॢ쥢)
   -- ***************************

   -----------------------------------------------------
   -- �㭪��, �����⢫���� ������஭��� (���室�騩)
   -- ��室 ����୮�� ��ॢ� ���᪠ tree
   -------------------------------------
   klpObh:: BTree -> [Int]
   klpObh tree | isEmpty tree = []
               | True         =   [root tree]
                               ++ klpObh (left tree)
                               ++ klpObh (right tree)

   ------------------------------------------------------
   -- �㭪��, �����⢫���� ���楢�� (���室�騩) ��室
   -- ����୮�� ��ॢ� ���᪠ tree
   -------------------------------
   lpkObh:: BTree -> [Int]
   lpkObh tree | isEmpty tree = []
               | True         =   lpkObh (left tree)
                               ++ lpkObh (right tree)
                               ++ [root tree]

   -----------------------------------------------------
   -- �㭪��, �����⢫���� ����� (ᬥ蠭��) ��室
   -- ����୮�� ��ॢ� ���᪠ tree
   -------------------------------
   lkpObh:: BTree -> [Int]
   lkpObh tree | isEmpty tree = []
               | True         =   lkpObh (left tree)
                               ++ [root tree]
                               ++ lkpObh (right tree)

   ---------------------------------------------
   -- �㭪��, �����⢫���� �����⢥��� ��室
   -- ����୮�� ��ॢ� ���᪠ tree
   -------------------------------
   klkpObh:: BTree -> [Int]
   klkpObh tree | isEmpty tree = []
                | True         =    [root tree]
                                ++ klkpObh (left tree)
                                ++ [root tree]
                                ++ klkpObh (right tree)

   -- ***********************
   -- �㭪樮���� �� ��ॢ���
   -- ***********************

   ----------------------------------------------
   -- ������஢���� �㭪樮���� map ��� ����୮��
   -- ��ॢ� ���᪠ tree
   ----------------------------------------
   mapBTree:: (Int ->Int) -> BTree -> BTree
   mapBTree f tree | isEmpty tree = nil
                   | True         = node (f (root tree))
                                         (mapBTree f (left tree))
                                         (mapBTree f (right tree))

   -------------------------------------------------
   -- ������஢���� �㭪樮���� filter ��� ����୮��
   -- ��ॢ� ���᪠ tree (�����頥��� ��ॢ�)
   -------------------------------------------
   filterBT:: (Int -> Bool) -> BTree -> BTree
   filterBT p tree 
        | isEmpty tree  = nil
        | p (root tree) = node (root tree) 
                               (filterBT p (left tree))
                               (filterBT p (right tree))
        | True          = delete (root tree)
                                 (node (root tree) 
                                       (filterBT p (left tree))
                                       (filterBT p (right tree)))

   --------------------------------------------------------
   -- �㭪��, ���������� �ᥢ����砩��� ����୮� ��ॢ�
   -- ���᪠, ᮤ�ঠ饥 m-n+1 楫�� �᫮ �� [0,a-1]
   --------------------------------------------------
   newRndTree:: Int -> Int -> Int -> BTree
   newRndTree n m a = consTree (newRndLst n m a)
     where
          -------------------------------------------
          -- �㭪��, ���������� ������ ���ᯨ᮪
          -- 楫�� �ᥫ
          --
          -- a ,a   ,...,a ,
          --  n  n+1      m
          --
          -- �ਭ�������� [0,a-1], �� ��⥭樠�쭮 ���-
          -- ����筮�� ᯨ᪠ �ᥢ����砩���� �ᥫ
          ------------------------------------------
          newRndLst n m a = 
            drop n (take (m+1) 
                     [x `mod` a | x <- iterate next_seed 23765492])
          ---------------------------------------------------------
          -- �㭪��, ��������� �ᥢ����砩��� �᫮
          ----------------------------------------------
          next_seed n = case test>0 of
                          True  -> test
                          False -> test+2147483647
               where test = 48271*lo-3399*hi 
                     hi   = n `div` 44488 
                     lo   = n `mod` 44488
   --------------------------------------------------------
   -- �㭪��, �������� (�� �����⠭��) �������� �᫮-
   -- ���� ᯨ᪠ � ������� ����୮�� ��ॢ� ���᪠
   ------------------------------------------------
   sort:: [Int] -> [Int]
   sort = lkpObh . consTree

   -- ************************************
   -- ���㠫����� ����୮�� ��ॢ� ���᪠
   -- ************************************

   --------------------------------------------------
   -- �㭪��, ����ࠦ���� ��ॢ� ���᪠ tree � ����
   -- ��ப� ᨬ����� (�� �맮�� l:=0)
   ------------------------------------
   drawTree:: BTree -> Int -> String
   drawTree tree l | isEmpty tree = ""
                   | True         = drawTree (right tree) (l+1) ++
                                    "\n" ++ 
                                    replicate (l*3) ' ' ++
                                    show (root tree) ++
                                    drawTree (left tree) (l+1)

   ----------------------------------------------------
   -- �㭪��, ����ࠦ���� ����୮� ��ॢ� ���᪠ tree
   -- �� ��࠭� ��ᯫ��
   -----------------------
   outTree:: BTree -> IO()
   outTree tree = putStr (drawTree tree 0 ++ "\n")

   ------------------------------------------------------
   -- �㭪��, �।�⠢����� ����୮� ��ॢ� ���᪠ tree
   -- � ���� "���筮��" ᯨ᪠, �.�. ��� 㪠�����  �����-
   -- ���� Node
   ------------------------
   outTree':: BTree -> IO()
   outTree' tree = putStr (show tree)

   --------------------------------------------------------
   -- �।�⠢����� ⨯� BTree � ����⢥ ��������� �����
   -- Show � ��।�������  ��⮤�, �।�⠢���饣� ����୮�
   -- ��ॢ� ���᪠ ��� 㪠����� ����� ��������� Node
   -----------------------------------------------------
   instance Show BTree
     where show tree
               | isEmpty tree = "Nil"
               | list (root tree)==tree 
                              = "(" ++ show (root tree) ++
                                " Nil" ++ " Nil" ++ ")"
               | True         = "(" ++ show (root tree) ++
                                " " ++ show (left tree) ++
                                " " ++ show (right tree) ++ ")"

   -- **************************************************
   -- � � � � � �  ॠ����樨 �㭪権 � �窨 �७�� ���
   -- (��㬥�⠬� �㭪権 ����� ���������� ������)
   -- **************************************************

   ----------------------------------------------
   -- ������஢���� �㭪樮���� map ��� ����୮��
   -- ��ॢ� ���᪠ tree
   -----------------------------------------
   mapBTree':: (Int ->Int) -> BTree -> BTree
   mapBTree' _ Nil          = nil
   mapBTree' f (Node x l r) = node (f x)
                                   (mapBTree' f l)
                                   (mapBTree' f r)
   ---------------------------------------------------------
   -- �㭪��, ����ࠦ���� ��ॢ� ���᪠ tree � ���� ��ப�
   -- ᨬ����� (�� �맮�� l=0)
   -- (� �ᯮ�짮������ ᮯ��⠢����� � ��ࠧ殬)
   ----------------------------------------------
   drawTree':: BTree -> Int -> String
   drawTree' Nil k          = ""
   drawTree' (Node x l r) k = drawTree' r (k+1)
                              ++ "\n" ++ replicate (k*3) ' '
                              ++ show x
                              ++ drawTree' l (k+1)
