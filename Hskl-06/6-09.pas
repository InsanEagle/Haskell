   { ���������� ���ᠭ�� � �맮�� �㭪樮���� Map. }
   {                                                 }
   { �㭪��, �ଠ��� ��ࠬ��஬ ���ன ���� }
   { �㭪��, ���뢠���� �㭪樮�����                }
   { ----------------------------------------------- }
   PROGRAM Primer_9;
      {$F+}                      { ���� ��������� }
      Uses CRT;
      const N=10;
      { ----------------------------- }
      type IA = Array[1..N] of Integer;
           F  = Function (X: Char)   : Char;
           F1 = Function (X: Integer): Integer;
           p  = ^IA;             { ��� "㪠��⥫� �� ���ᨢ" }
      { ---------------------------------------------------- }
      var  Fun  : F;
           Fun1 : F1;
           g    : p;             { �����⥫� �� ���ᨢ }
           w    : p;             { �����⥫� �� ���ᨢ }
           i    : Integer;
           Slovo: String;
  { ------------------------- }
   FUNCTION R (x: Char): Char;
   BEGIN
      R := UpCase(x);
    { R := Chr(Ord(x)+32); }
   END;
  { -------------------------------- }
   FUNCTION R1 (x: Integer): Integer;
   BEGIN
      R1 := x Div 2
   END;

  { --------------------------------------- }
   FUNCTION Map (G: F; Str: String): String;
   { ������஢���� �㭪樮���� map }
   { ----------------------------- }
      var i: Integer;
          r: String;
   BEGIN
      r:='';
      For i:=1 to Length(Str) do
        r := Concat(r,G(Str[i]));
      Map := r
   END;
  { ------------------------------- }
   FUNCTION Map_1 (G: F1; d: p): p;
   { ������஢���� �㭪樮���� map }
   { ----------------------------- }
      var i: Integer;
   BEGIN
      For i:=1 to N do
        d^[i] := G(d^[i]);
      Map_1 := d
   END;
  { --- }
   BEGIN
      Randomize;
      Write('�������� ���ᨢ�: ');
      For i:=1 to N do
        begin g^[i]:=Random(80); Write(g^[i]:2,' ') end;
      WriteLn;
      w:=Map_1(R1,g);
      Write('�������       : ');
      For i:=1 to N do Write(w^[i]:2,' ');
      WriteLn; WriteLn;
      { ------------- }
      Slovo:='';
      For i:=1 to 10 do
        Slovo := Slovo + Chr(Ord('a')+Random(5));
      WriteLn('�����           : ',Slovo);
      WriteLn('�������       : ',Map(R,Slovo));
      WriteLn;
      Repeat until KeyPressed
   END.
