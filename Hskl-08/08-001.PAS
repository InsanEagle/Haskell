   { ����������:                                 }
   {  (1) ����権, ����ண�� �� ��஬� ��㬥��� }
   {      (�⫮������ ���᫥���);                 }
   {  (2) ���ࣨ��� ���᫥���                    }
   { --------------------------------------------- }
   PROGRAM Primer_1;
      Uses CRT;
      var x: Real;
          y: Integer;
   BEGIN
      x:=0; y:=1;
      { ------------------------------ }
      { ����� ���ࣨ�� ���᫥���  }
      { ------------------------------ }
      WriteLn('(1) ',TRUE OR (x=1/0.001));
      if y>0
        then WriteLn ('(2) ',y)
        else begin x:=1/0.001; WriteLn (x) end;
      Repeat until KeyPressed
   END.
