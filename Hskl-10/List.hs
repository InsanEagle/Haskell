   -- ���������� �࠭��ଠ樨 ��������஢
   -- **********************************************
   -- ������஢���� ᯨ᪠, ᮤ�ঠ饣� ���� �������
   -------------------------------------------------
   list1 = flip (:) []

   -- **********************************************
   -- ������஢���� ᯨ᪠, ᮤ�ঠ饣� ��� �������
   -------------------------------------------------
   list2 = (.) (flip (:)) (flip (:) [])

   -- **********************************************
   -- ������஢���� ᯨ᪠, ᮤ�ঠ饣� ��� �������
   -------------------------------------------------
   list2' = flip ((.) (flip (:)) (flip (:) []))