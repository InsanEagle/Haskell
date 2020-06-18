   -- ���������� ��宦����� ����⢥���� ��୥� ����-
   -- �⭮�� �ࠢ����� � �ᯮ�짮������ ࠧ����� ᨭ-
   -- ⠪��᪨� ��ਠ�⮢ ��ଫ���� ���몠���:
   --   (1) where ...
   --   (2) let ... in
   ---------------------------------------------------
   twoRoots:: Float -> Float -> Float -> (Float,Float)
   twoRoots a b c = (
                      (-b-sqrt(b*b-4.0*a*c))/2.0/a,
                      (-b+sqrt(b*b-4.0*a*c))/2.0/a
                    )
   ----------------------------------------------------
   twoRoots1:: Float -> Float -> Float -> (Float,Float)
   twoRoots1 a b c = (d-e,d+e)
        where d = -b/(2.0*a)
              e = sqrt(b^2-4.0*a*c)/(2.0*a)
   ----------------------------------------------------
   twoRoots2:: Float -> Float -> Float -> (Float,Float)
   twoRoots2 a b c = let d = -b/(2.0*a)
                         e = sqrt(b^2-4.0*a*c)/(2.0*a)
                     in (d-e,d+e)
   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------------------
   test =   twoRoots  1 (-5) 6 == ( 2.0, 3.0)
         && twoRoots  1  5   6 == (-3.0,-2.0)
         && twoRoots1 1 (-5) 6 == ( 2.0, 3.0)
         && twoRoots  1  5   6 == (-3.0,-2.0)
         && twoRoots2 1 (-5) 6 == ( 2.0, 3.0)
         && twoRoots  1  5   6 == (-3.0,-2.0)