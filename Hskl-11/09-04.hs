   -- ���������� �८�ࠧ������ ����୮�� ��ॢ� ᯥ-
   -- 樠�쭮� ��� � ᯨ᮪ � ���⭮.
   -- 
   -- �����: ����楢� �.�., ���檨� �.�.
   --                 (01.03.2013,05.03.2013,09.03.2013)
   -----------------------------------------------------
   import Tree
   -------------------------------------------------------
   -- �㭪�� ����஥��� ����୮�� ��ॢ� �� ᯨ�� ���設
   -- ����祭��� � १���� ��室� ��㣮�� ����୮�� ��-
   -- ॢ� � �ਭ�
   ------------------------
   breadth lst = brfi lst 0
   --------------------------------
   brfi lst i | i>=length lst = nil
              | True          = node (lst !! i)
                                  (brfi lst (2 * i + 1))
                                  (brfi lst (2 * i + 2))
   -----------------------------------------------------
   -- �㭪�� �����頥� ᯨ᮪ ���設, ����祭��� �
   -- १���� ��室� ����୮�� ��ॢ� � �ਭ�
   ---------------------------------------------- 
   masBr tree | isEmpty tree = []
              | True         = [root tree] ++ 
                               uroven tree ++ 
                               masBr1 tree
   ---------------------------------------
   masBr1 tree | isEmpty tree = []
               | True      = uroven (left tree) ++ 
                             uroven (right tree) ++
                             masBr1 (left tree) ++
                             masBr1 (right tree)
   ---------------------------------------------
   uroven tree | isEmpty (left tree) && 
                 isEmpty (right tree) = []
               | isEmpty (left tree)  = [root (right tree)]
               | isEmpty (right tree) = [root (left tree)]
               | True = root (left tree):[root (right tree)]
   -- ******************************************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   l1 = map toInt [13]
   l2 = map toInt [13,10]
   l3 = map toInt [13,10,9]
   l4 = map toInt [13,10,9,4]
   l5 = map toInt [13,10,9,4,3]
   l6 = map toInt [13,10,9,4,3,7]
   l7 = map toInt [13,10,9,4,3,7,8]
   l8 = map toInt [13,10,9,4,3,7,8,10,11,12,13,14,15,16,17]
   l9 = map toInt [1..22]
   ----------------------
   tree1 = breadth l1
   tree2 = breadth l2
   tree3 = breadth l3
   tree4 = breadth l4
   tree5 = breadth l5
   tree6 = breadth l6
   tree7 = breadth l7
   tree8 = breadth l8
   tree9 = outTree $ breadth l9
   ----------------------------
   test = writeFile "test.txt" 
          ("��室 ����୮�� ��ॢ� � �ਭ�: "++show l1++"\n"++
           "��ॢ�: "++show tree1++"\n"++(drawTree tree1 0)++"\n"++
           "\n-------------------------------------------------\n"++
           "��室 ����୮�� ��ॢ� � �ਭ�: "++show l2++"\n"++
           "��ॢ�: "++show tree2++"\n"++(drawTree tree2 0)++"\n"++
           "\n-------------------------------------------------\n"++
           "��室 ����୮�� ��ॢ� � �ਭ�: "++show l3++"\n"++
           "��ॢ�: "++show tree3++"\n"++(drawTree tree3 0)++"\n"++
           "\n-------------------------------------------------\n"++
           "��室 ����୮�� ��ॢ� � �ਭ�: "++show l4++"\n"++
           "��ॢ�: "++show tree4++"\n"++(drawTree tree4 0)++"\n"++
           "\n-------------------------------------------------\n"++
           "��室 ����୮�� ��ॢ� � �ਭ�: "++show l5++"\n"++
           "��ॢ�: "++show tree5++"\n"++(drawTree tree5 0)++"\n"++
           "\n-------------------------------------------------\n"++
           "��室 ����୮�� ��ॢ� � �ਭ�: "++show l6++"\n"++
           "��ॢ�: "++show tree6++"\n"++(drawTree tree6 0)++"\n"++
           "\n-------------------------------------------------\n"++
           "��室 ����୮�� ��ॢ� � �ਭ�: "++show l7++"\n"++
           "��ॢ�: "++show tree7++"\n"++(drawTree tree7 0)++"\n"++
           "\n-------------------------------------------------\n"++
           "��室 ����୮�� ��ॢ� � �ਭ�: "++show l8++"\n"++
           "��ॢ�: "++show tree8++"\n"++(drawTree tree8 0)++"\n")
   ---------------------------------------------------------------
   test1 = do
             outTree tree1
             putStr ("\n"++ show (masBr tree1))
   test2 = masBr tree2
   test3 = masBr tree3
   test4 = do
             outTree tree8
             putStr ("\n" ++ show (masBr tree8))