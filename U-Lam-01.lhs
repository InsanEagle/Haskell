>  import Prelude hiding (curry,uncurry)

   �2�����������0 �21.
   �2�������� ����������� �7l�2-�����������0 �2� ����� Haskell

   �_�1�ᯮ����⥫�� �������.�0:
��1.0


��1.2
   �_�1�᭮��� �������.�0:
��1.0



��1.2

                      �2������������� ��������
   �2��।�������0 (�1ᮤ�ঠ⥫쭮��0) (�� [����,����ᮭ,1993,�.121]).
��1.1
   �7l�0-�_�1���᫥����.�0 - �� ���᫥���  ���������  (�1����ﭭ���0)  �㭪権,
���஥ �।��⠢���:
   (1) ��⮤ �।�⠢����� �㭪権 �  ᯮᮡ��  ��  �������஢����,
����� ���砥� �㭪樨 ��3 �0�裡 � �� �1�������⨢���0 (����.  �1applica-
�1tion�0 - �1�ਫ�������0) ����������. ���⮬� �1����������0 (�ਬ������ �㭪-
樨 � ��㬥���) ���� ��室��� ����樥� �7l�0-���᫥���;
   (2) ������⢮  �ࠢ��  �뢮�� ��� ᨭ⠪��᪮�� �८�ࠧ������
�㭪権.
��1.2

                      �2���⨯�����0 �7l�0-�2���᫥���
                      �2��� �ଠ�쭠� ��⥬�
                       �21.�0 �3����0 �7l�1-�3���᫥���
   ��।���� ���7 l�0-���᫥��� ��� �� � �����஬ ��䠢��.
   �2��।�������0.
��1.1
   �_�1��䠢�� ���⨯������. �7l�1-�_���᫥����.�0 (������砥��� �7A��0)  -  ��  ���-
���⢮
��1.0

   �7A���+�0G�41�7u�0G�42�7u�0G�43�7u�0G�44�0,

��1.1
   �2(1)�0 G�41��+�0{x�41�0,x�42�0,x�43�0,...} - �1������⢮ �।����� ��६������0;
   �2(2)�0 G�42��+�0{=}  - �1������⢮ �।������ ᨬ������0;  "=" - ���嬥���
�।����� ᨬ��� (�⠥���: "�1ࠢ���0" ��� "�1���������0");
   �2(3)�0 G�43��+�0{�7l�0} - �1������⢮ �����᪨�  ᨬ������0;  "�7l�0"  -  �����᪨�
ᨬ��� (�⠥���: "�1�ﬡ���0", "�1����ࠪ���0", "�1�㭪�� ���0");
   �2(4)�0 G�44��+�0{�2(�0,�2)�0,�2.�0} - �1������⢮ �ᯮ����⥫��� ᨬ������0;  ᨬ��� "�2(�0"
�⠥���: "�1���뢠��� ᪮����0",  ᨬ��� "�2)�0" �⠥���:  "�1����뢠���
�1᪮����0"), ᨬ��� "�2.�0" �⠥���: "�1�窠�0", "�1����� �����頥��0".

��1.2
   ��।���� ����⨢��  ����⨥ "�7l�0-�1���0" ("�1�ﬡ��-���0" ��� ����
"�1���0").
   � ����몥,  �।�����祭��� ��� ���ᠭ�� �7l�1-�0���᫥���, �7l�0-���
�㤥� ��������� �1�ய��묨 ��⨭᪨�� �㪢����0,  � �।���� ���-
����� - �1����묨 ��⨭᪨�� �㪢����0.
   �2��।�������0.
��1.1
   �2(1)�0 �।���� ��६���� ����� �7l�0-�_�1�ଠ���.�0.
   �2(2)�0 �᫨ M � N - �7l�0-���,  � �2(�0MN�2)�0 (�⠥���: "�1����������0 �7l�1-��-
�1��� M ��0 �1N�0") ���� �7l�0-�_�1�ମ��.�0.
   �2(3)�0 �᫨ M - �7l�0-��, � � - �।��⭠� ��६�����, � �2(�7l�0�.��2)�0 (�-
⠥���: "�1����ࠪ���0 �7l�1-�ଠ M�0") ���� �7l�1-�_�ମ��.�0.
   �� �⮬ x �㤥� ���뢠�� �_�1��६����� ����ࠪ樨�.�0,  � �7l�1-�0��  M  -
�_�1⥫�� ����ࠪ樨�.�0.
   �2(4)�0 ��㣨� �7l�0-�ମ�, �஬� ����஥���� �� ��.(1)-(3), ���.

��1.2

>  data Lambda =   Var String        -- �।��⭠� ��६�����
>                | App Lambda Lambda -- ���������
>                | Lam String Lambda -- ����ࠪ��
>    deriving Eq

   -- ****************************************
   -- �㭪樨 ��� �।�⠢����� �ﬡ��-�ମ�:
   -------------------------------------------

>  lam41 = App (Lam "x" (App (Var "x") (Var "y")))
>              (Lam "y" (Var "y"))
   
>  lam42 = Lam "x" (Lam "y"
>                    (App (Var "z")
>                         (Lam "z" (App (Var "z")
>                                  (Lam "x" (Var "y"))))))
   
>  lam43 = App (Lam "y" (App (App (Var "x") (Var "y"))
>                            (Var "z")))
>              (Lam "z" (Lam "x" (App (Var "x") (Var "y"))))
   
>  lam44 = App (Lam "x" (Lam "y" (App (Var "x")
>                                     (Lam "z" (App (Var "y")
>                                                   (Var "z"))))))
>              (Lam "x" (Lam"y" (Var "z")))

   �_�1�����襭�� �� ������祭����.�0 �7l�_�1-�ମ��.�0.
��1.1
   �� ����� �7l�0-�ମ� �  ����몥  ���  �ᯮ�������  ᫥���騥
�_�1ᮣ��襭�� �� ������祭����.�0,  ��������騥 �� �����  �7l�0-�ମ� ����-
���� ᪮��� � �������騥�� ᨬ�����7 l�0:
   (�) �_�1ᮣ��襭�� � ����⠭������� ᪮��� �� ���樠⨢���� ������.�0:
᫮�� ����몠
��1.0

   M�41�0M�42�0...M�4n�0

���� ᮪��񭭮� ������� �7l�0-�ଠ

   (...((M�41�0M�42�0)M�43�0)...M�4n�0);

��1.1
   (�) �_�1ᮣ��襭�� �� ���᪠��� ����������� ᨬ������.�0 �7l�0 �_�1� � �����-
�_�1�������� ᪮��� �� ���樠⨢���� ��ࠢ��.�0: ᫮�� ����몠
��1.0

   �7l�0x�41�0��42�0...��4n�0.M

���� ᮪��񭭮� ������� �7l�0-�ଠ

   (�7l�0x�41�0.(�7l�0��42�0.(...(�7l�0��4n�0.M)...)))

��1.1
(�������, �� "⥫� ����ࠪ権 ��������� �1���ࠢ��0 ���ᨬ��쭮 ��-
����");
   (�) �_�1������饥 ᮣ��襭���.�0: ᫮�� ����몠
��1.0

   �7l�0x�41�0��42�0...��4n�0.M�41�0M�42�0...M�4k

���� ᮪��񭭮� ������� �7l�0-�ଠ

   (�7l�0x�41�0.(�7l�0��42�0.(...(�7l�0��4n�0.(...((M�41�0M�42�0)M�43�0)...M�4k�0))...))).

��1.2
   �2�ਬ����0.
��1.0

   �����������������������������������������������������Ŀ
   �              �7l�3-���0              ��3�����񭭠� �������0�
   �                                  �   �3� ����몥�0    �
   �����������������������������������������������������͵
   � (((M�41�0M�42�0)M�43�0)M�44�0)                   � M�41�0M�42�0M�43�0M�44�0         �
   �                                  �                  �
   � (((xz)y)z)                       � xzyz             �
   �                                  �                  �
   � ((xz)(yz))                       � xz(yz)           �
   �                                  �                  �
   � (�7l�0x�41�0.(�7l�0��42�0.(�7l�0��43�0.M)))              � �7l�0x�41�0��42�0��43�0.M        �
   �                                  �                  �
   � (�7l�0x�41�0.(�7l�0��42�0.(�7l�0��43�0.(((M�41�0M�42�0)M�43�0)M�44�0)))) � �7l�0x�41�0��42�0��43�0.M�41�0M�42�0M�43�0M�44�0 �
   �������������������������������������������������������

��1.2
   �2��।�������0 (�1�.�����0).
��1.1
   �������� "�7=�0" ����몠 �㤥� ��������� ����୮� �⭮襭�� "�_�1��-
�_�1��᪮�  ࠢ���⢮�.�0 �7l�1-�_�ମ��.�0",  ��⠭�������饥 �� ������ ᨭ⠪�-
�᪮� ᮢ�������.
   ��㣨�� ᫮����, �㤥� 㯮�ॡ���� ������
��1.0

   M�7=�0N

��� ������祭�� �1���㪢������ ᮢ��������0 �7l�0-�ମ� M � N.

��1.2
   �2��।�������0 (�� [��७�ॣ�,1985,�.35]).
��1.1
   �_�1�������.�7 l�0-�_�1�ଠ�.�0 �1M�0 (������砥��� �M�) ���뢠���� ������⢮ ᨬ��-
��� � ������7 l�0-�ଠ.

��1.2
   �2��।�������0.
��1.1
   �_�1�������.�0 �7l�0-�_�1�ଠ�.�0 ���뢠���� �㭪�� �1rank�0:�7L�0��76��N�0, ��।��塞�� ᫥��-
�騬 ����⨢�� ��ࠧ��:
��1.0

           �7(�00, �᫨ M�7=�0x�4i�0, i�7���N�0;
           �
   �1rank�0(M)��+�7*�1rank�0(N�41�0)+�1rank�0(N�42�0), �᫨ M�7=�2(�0N�41�0N�42�2)�0, N�41�0N�42�7�L�0;
           �
           �79�01+�1rank�0(N), �᫨ M�7=�2(�7l�0x.N�2)�0, N�7�L�0.

��1.2
   �_�1�����襭�� �� ������祭����.�0.
��1.1
   ������稬 �7L�0 - ������⢮ �7l�0-�ମ�.

��1.2
   ����� M,N�7�L�0.
   �2��।�������0.
��1.1
   �_�1���㫮� ���⨯������. �7l�1-�_���᫥����.�0 (�7l�0-�_�1��㫮��.�0 ��� ����  �_�1���-
�_�1����.�0) ���뢠���� ᫮�� ����:
��1.0

   M=N.

��1.2
   �_�1�����襭�� �� ������祭����.�0.
��1.1
   ������稬 F�7��0 - ������⢮ �7l�0-���.

��1.2
   �2��।�������0.
��1.1
   �_�1��몮��.�0 �7l�1-�_���᫥����.�0 (��� �7l�1-�_�몮��.�0) ���뢠���� ���⥦
��1.0

   L�7���+�2<�7A��0,�7L�0,F�7��2>�0.

��1.2
                �22.�0 �3�������� � �易��� ��६����
   �2��।�������0 (�� [��७�ॣ�,1985,�.37]).
��1.1
   �_�1������⢮ ����ମ��. �7l�0-�_�1�ଠ�. M�0  (������砥���  �1Sub�0(M))  ��।����
����⨢�� ᫥���騬 ��ࠧ��:
��1.0

   �2(1)�0 �1Sub�0(x)��+�0{x}, �᫨ x�7��0G�41�0;

          �4��0      �4�
   �2(2)�0 �1Sub�0��2(�0M�41�0M�42�2)�0���+�0{M�41�0M�42�0}�7u�1Sub�0(M�41�0)�7u�1Sub�0(M�42�0), �᫨ M�41�7�L�0, M�42�7�L�0;
          �4��0      �4�
          �4��0      �4�
   �2(3)�0 �1Sub�0��2(�7l�0x.M�2)�0���+�0{�7l�0x.M}�7u�1Sub�0(M), �᫨ M�7�L�0.
          �4��0      �4�

��1.2

>  sub:: Lambda -> [Lambda]
>  sub (Var y)     = povto [Var y]
>  sub (App e1 e2) = povto ([App e1 e2] ++ sub e2 ++ sub e1)
>  sub (Lam y e)   = povto ([Lam y e] ++ sub e)

>  -- �㭪樨 ��� �।�⠢����� ������-�ମ�:

>  lam11 = Lam "x" (App (App (Var "x") (Var "y"))
>                       (Lam "z" (Var "y")))
>  lam12 = Lam "x" (App (Var "y") (Var "x"))

>  test1 =   show (sub lam11)
>                == "[\\x.xy(\\z.y),xy(\\z.y),\\z.y,y,xy,x]"
>         && show (sub lam12)
>                == "[\\x.yx,yx,x,y]"

   �2��।�������0.
��1.1
   �7l�0-�_�1����ମ��.�0 �1M �7l�0-�_�1�ଠ�.�0 �1N�0 �㤥� ���뢠�� �7l�0-�� M�7��1Sub�0(N).

��1.2
   �7l�0-��� �����  �1᫮����  �  ��䠢�� ���⨯����� �7l�1-���᫥����0,
���⮬� ��� �7l�0-�ମ� ��।����� �⮡ࠦ����  "�1�宦�����  �।��⭮�
�1��६����� � �7l�1-���0".
   �2��।�������0.
��1.1
   �2(1)�0 �_�1��易��� �宦������ �।��⭮� ��६������. x �_��.�0 �7l�0-�_�1���. M�0  ��-
�뢠���� �宦����� �।��⭮� ��६����� x � �7l�0-�� M, ��� ���ண�
�1��������0 ⠪�� �7l�0-�� N, ��
��1.0

   �2(�7l�0x.N�2)�7��1Sub�0(M).

��1.1
   �2(2) �_�1�������� �宦������ �।��⭮� ��६������. x �_��.�0 �7l�0-�_�1���.�0 �1M�0  ��-
�뢠���� �宦����� �।��⭮� ��६����� x � �7l�0-�� M, ��� ���ண�
�� ������� ⠪��� �7l�0-�ଠ N, ��
��1.0

   �2(�7l�0x.N�2)�7��1Sub�0(M).

��1.2
   �2��।�������0.
��1.1
   �2(1)�0 �_�1��������� �।��⭮� ��६������.�0 �7l�0-�_�1�ଠ�. M�0  ���뢠����  �।-
��⭠�  ��६�����,  ������  ���  ��  ����  ᢮������ �宦����� �
�7l�0-�� M.
   �2(2)�0 �_�1��易���� �।��⭮� ��६������.�0 �7l�0-�_�1�ଠ�. M�0  ���뢠����  �।-
��⭠� ��६�����,  ������ ���  ��  ����  �易����  �宦�����  �
�7l�0-�� M.

��1.2
   �2�ਬ����0.
��1.1
   �21.�0 � �7l�0-�ଥ �2(�7l�0z.x�2)(�7l�0y.yx�2(�7l�0x.yzx�2))�0 ᠬ�� ����� �宦����� z �-
���� �易���, � ᠬ�� �ࠢ�� - ᢮�����; ��� ᠬ�� ����� �宦��-
��� x ����� ᢮����묨, � ��� ᠬ�� �ࠢ�� - �易��묨; �� ��
�宦����� y ����� �易��묨.
   ��������� �।�⠢�� ⠪:
��1.0

     �7^   �0 �7 ^ ^ �0 �7 ^ ^ ^�0   �1��易��� ��६����
   �2(�7l�_�0z�..x�2)(�7l�_�0y�..yx�2(�7l�_�0x�..yzx�2))
                ����0�����0�����
    ����0�����0�����0 ����0��������0��������
       �7% �0 �7    %     %�0    �1�������� ��६����

��1.1
   �22.�0 � �7l�0-�ଥ �2(�0x�2(�7l�0x.x�2))�0 �।��⭠� ��६����� x ���� ������-
����� � ᢮������, � �易����.

��1.2

>  free:: [Char] -> Lambda -> Bool
>  free x (Var y)     = x==y
>  free x (App e1 e2) = free x e1 || free x e2
>  free x (Lam y e)   = x/=y && free x e

>  -- �㭪樨, �।�⠢���騥 ������-���:

>  lam21 = Lam "x" (Lam "y" (Lam "z"
>                (App (Var "x") (App (Var "y") (Var "z")))))
>  lam22 = Lam "x" (Lam "y" (Lam "z"
>                (App (App (Var "x") (Var "z")) (Var "y"))))
>  lam23 = Lam "x" (Lam "y"
>                (App (App (Var "x") (Var "y")) (Var "y")))
>  lam24 = App (Lam "x" (App (Var "x") (Var "y")))
>              (Lam "y" (Var "y"))
>  lam25 = Lam "x" (App (Var "b") (App (Var "f")
>                  (App (Var "a") (Var "x"))))

>  test2 =   not (free "x" lam21) && not (free "y" lam21)
>         && not (free "z" lam21) && not (free "x" lam22)
>         && not (free "y" lam22) && not (free "z" lam22)
>         && not (free "x" lam23) && not (free "y" lam23)
>         && not (free "x" lam24) &&      free "y" lam24
>         && not (free "x" lam25) &&      free "b" lam25
>         &&      free "f" lam25  &&      free "a" lam25

>  notfree:: [Char] -> Lambda -> Bool
>  notfree x (Var y)     = False
>  notfree x (App e1 e2) = notfree x e1 || notfree x e2
>  notfree x (Lam y e)   = x==y || notfree x e

>  -- �㭪樨 ��� �।�⠢����� �ﬡ��-�ମ�:

>  lam31 = App (Lam "v" (Var "x"))
>              (Lam "y" (App (App (Var "y") (Var "x"))
>                            (Lam "x" (App (App (Var "y") (Var "v"))
>                                          (Var "x")))))

>  lam32 = Lam "x" (Lam "y" (App (Var "z")
>                           (Lam "z" (App (Var "z")
>                                         (Lam "x" (Var "y"))))))

>  test3 =   notfree "v" lam31  -- lam1=(\v.x)(\y.yx(\x.yvx))
>         && notfree "y" lam31
>         && notfree "x" lam31
>         && notfree "x" lam32  -- lam2=\x.\y.z(\z.z(\x.y))
>         && notfree "y" lam32
>         && notfree "z" lam32

   �2��।�������0 (�� [��७�ॣ�,1985,�.36]).
��1.1
   �_�1������⢮�0 �1᢮������ ��६������. �7l�1-�_�ଠ�.�0  �1M�0  (������砥���  FV(M))
��।���� ����⨢��:
��1.0

   �2(1)�0 FV(x)��+�0{x};  �2(2)�0 FV(MN)��+�0FV(M)�7u�0FV(N);  �2(3)�0 FV(�7l�0x.M)��+�0FV(M)\{x}.

��1.2

   -- ���������� �㭪樨, �������饩 ������⢮ 
   -- ᢮������ ��६����� ��������� �ଠ
   -- ************************************

>  mn_free:: Lambda -> [[Char]]
>  mn_free (Var y)     = [y]
>  mn_free (Lam y e)   = povto (filter (y/=) (mn_free e))
>  mn_free (App e1 e2) = povto (mn_free e1 ++ mn_free e2)

>  test4 =   mn_free lam41 == ["y"]
>         && mn_free lam42 == ["z"]
>         && mn_free lam43 == ["x","z","y"]
>         && mn_free lam44 == ["z"]

   �2��।�������0.
��1.1
   �_�1������⢮�0 �1�易���� ��६������. �7l�1-�_�ଠ�.�0  �1M�0  (������砥���  CV(M))
��।���� ����⨢��:
��1.0

   �2(1)�0 CV(x)��+�7'�0;  �2(2)�0 CV(MN)��+�0CV(M)�7u�0CV(N);  �2(3)�0 CV(�7l�0x.M)��+�0{x}�7u�0CV(M).

��1.2
   �2�ਬ����0.
��1.0

        �4��0                     �4�
   �21. �0FV��2(�7l�0z.x�2)(�7l�0y.yx�2(�7l�0x.yzx�2))�0���+�0FV(�7l�0z.x)�7u�0FV(�7l�0y.yx(�7l�0x.yzx))��+
        �4��0                     �4�
       �4��0       �4��0 �4��0                  �4��0 �4��0       �4��0 �4��0           �4�
      ��+�0�{x}\{z}��7u�0�FV(yx(�7l�0x.yzx))\{y}���+�0�{x}\{z}��7u�0�{y,x,z}\{y}�={x,z}.
       �4��0       �4��0 �4��0                  �4��0 �4��0       �4��0 �4��0           �4�
        �4��0                     �4��0   �4��0    �4��0   �4��0             �4�
   �22. �0CV��2(�7l�0z.x�2)(�7l�0y.yx�2(�7l�0x.yzx�2))�0���+�0CV��7l�0z.x��7u�0CV��7l�0y.yx(�7l�0x.yzx)���+
        �4��0                     �4��0   �4��0    �4��0   �4��0             �4�
                       �4��0          �7)
      ��+�0{z}�7u�0CV(x)�7u�0{y}�7u�0CV�yx(�7l�0x.yzx)�72��+�0{z,y}�7u�0{x}={x,y,z}.
                       �4��0          �70

��1.2
              �3����� � �ਬ�ࠬ� ����� �ࠦ�����
��1.1
   ��. �ਬ�� 1.

��1.2

   -- ���������� �㭪樨, �������饩 ������⢮
   -- �易���� ��६����� ��������� �ଠ.

>  mn_notfree:: Lambda -> [[Char]]
>  mn_notfree (Var y)     = []
>  mn_notfree (App e1 e2) = povto (mn_notfree e1 ++ mn_notfree e2)
>  mn_notfree (Lam y e)   = povto ([y] ++ mn_notfree e)

>  -- �㭪樨 ��� �।�⠢����� �ﬡ��-�ମ�:

>  lam51 = App (Lam "x" (App (Var "x") (Var "y")))
>              (Lam "y" (Var "y"))

>  lam52 = Lam "x" (Lam "y" (App (Var "z")
>                                (Lam "z" (App (Var "z")
>                                         (Lam "x" (Var "y"))))))

>  lam53= App (Lam "y" (App (App (Var "x") (Var "y"))
>                           (Var "z")))
>             (Lam "z" (Lam "x" (App (Var "x") (Var "y"))))

>  lam54 = App (Lam "x" (Lam "y" (App (Var "x")
>                                     (Lam "z" (App (Var "y")
>                                                   (Var "z"))))))
>              (Lam "x" (Lam"y" (Var "z")))

>  test5 =   mn_notfree lam51 == ["x","y"]
>         && mn_notfree lam52 == ["x","y","z"]
>         && mn_notfree lam53 == ["y","z","x"]
>         && mn_notfree lam54 == ["x","y","z"]

   �2��।�������0 (�1�������0) (�� [��७�ॣ�,1985,�.36]).
��1.1
   �_�1��������஬�.�0 (�_�1���������. �7l�0-�_�1�ମ��.�0) ������ �7l�0-�� M, ��� ���ண�
��1.0

   FV(M)=�7'�0.

��1.2
   �3����砭���0.
��1.1
   ������⢮ �易���� ��६����� ��� ��������� BV (����.  �1boun-
�1ded�0 - �1��࠭�祭���0) ��� CV (����. �1combined variable�0 - �易���� ��-
६�����).

��1.2
                     �23.�0 �3�������0 �3"����⠭����"
   ��।���� �㭤����⠫��� ��� �7l�0-���᫥��� ������
��1.0

   �7(�0 �7)�0�
   ���72�0 :�7L&�0G�41�7&L�0��76L�0,
   �79�0 �70�0�

��1.2
������ �㤥� ���뢠�� �_�1����⠭������.�0 �7l�0-�_�1�ଠ�.�0 �1N�0 �_�1����� ��� ᢮������
�_�1�宦����� �।��⭮� ��६������.�0 �1x�0 �_�1��.�0 �7l�0-�_�1���.�0 �1M�0.
   ����� x, y, z - �㪢� ����몠, ��������騥  �।����  ���-
�����, M,M�41�0,M�42�0,N�7�L�0.
   �2��।�������0 (�1�� �.��0.�1�����0)
               [��७�ॣ�,1985,�.89,567; ������,1991,�.122].
��1.1
   �_�1������� ����⠭�����.�0 �7l�0-�_�1�ଠ�.�0 �1N�0 �_�1����� ��� ᢮������  �宦�����
�_�1�।��⭮� ��६������.�0 �1x�0 �_�1��.�0 �7l�0-�_�1���.�0 �1M�0 ������砥��� (M)�5x�4N�0 � ��।����-
�� ����⨢�� ᫥���騬 ��ࠧ��:
��1.0

        �4��0 �4�x
   �2(p�41�2) �0�x� ��+�0N;
        �4��0 �4��5N
        �4��0 �4�x�0                         �4��0 �4�x
   �2(p�42�2)�0 �M� ��+�0M, �᫨ x�7��0FV(M);  �2(p�42�2')�0 �y� ��+�0y, �᫨ x �1����᪨�0 ��
        �4��0 �4��5N�0                         �4��0 �4��5N�0    ᮢ������ � y.
        �4��0    �4�x�0 �4��0  �4�x��0  �4�x
   �2(p�43�2)�0 �M�41�0M�42�0� ��+�0�M�41�0� �M�42�0� ;
        �4��0    �4��5N�0 �4��0  �4��5N�4��0  �4��5N
        �4��0    �4�y
   �2(p�44�2)�0 ��7l�0y.M� ��+�7l�0y.M;
        �4��0    �4��5N

   �2(p�45�2)�0 �_�1����⠭���� ��� ���������.�0:

        �4��0    �4�x�0    �4��0 �4�x
        ��7l�0y.M� ��+�7l�0y.�M� , �᫨ y�7��0FV(N);
        �4��0    �4��5N�0    �4��0 �4��5N

   �2(p�46�2)�0 �_�1���࠭���� �������� ��� ��६����� �� ����⠭�����.�0:

        �4��0    �4�x�0    �4���0 �4�y�x
        ��7l�0y.M� ��+�7l�0z.��M� � , �᫨ y�7��0FV(N), z�7��0FV(M)�7u�0FV(N).
        �4��0    �4��5N�0    �4���0 �4��5z�4��5N

��1.2
   �2�ਬ����0 (�1�믮������ ����樨 "����⠭����"�0).
��1.0

      �4��0      �4�y�0 �5(p6)�0   �4��0   �4�y�0 �5(p3,p1,p2,p2')
   �21.�0 ��7l�0x.fxy�   ��+�0  �7l�0z.�fzy�        ��+�0       �7l�0z.fzx.
      �4��0      �4��5x�0        �4��0  �5 �4��5x

      �4��0     �4�x�0 �5(p6)   �4���0  �4�y�x��5(p3,p1,p2')�0   �4��0  �4�x�0 �5(p3)
   �22.�0 ��7l�0y.yx�   ��+�0  �7l�0z.��yx� � � �4    ��+�4 �0  �4  �7l�0z.�zx� �4  ��+
      �4��0     �4��5y�0        �4���0  �4��5z�4��5y�4��0    �4       �0  �4 ��0  �4��5y

                    �4��0 �4�x��0 �4�x�0 �5(p2')�0    �4��0 �4�x�0 �5(p1)
                ��+�7l�0z.�z� �x�    ��+�0  �7l�0z.z�x�   ��+�0  �7l�0z.zy.
                    �4��0 �4��5y�4��0 �4��5y�0          �4��0 �4��5y

      �4��0    �4�x �5(p5)�0   �4��0 �4�x�0 �5(p1)
   �23.�0 ��7l�0y.x�   ��+�0  �7l�0y.�x�  �4 ��+�4  �7l�0y.N, �᫨ y�7��0FV(N).
      �4��0    �4��5N�0        �4��0 �4��5N

      �4��0    �4�x �5(p6)�0   �4���0 �4�y�x��5(p2')�0   �4��0 �4�x�0 �5(p1)
   �24.�0 ��7l�0y.x�   ��+�0  �7l�0z.��x� � � �4 ��+�4  �7l�0z.�x� �4  ��+�4  �7l�0z.N, �᫨ y�7��0FV(N).
      �4��0    �4��5N�0        �4���0 �4��5z�4��5N�4��0    �4    ��0 �4��5N

      �4��0     �4�x�0 �5(p5)�0   �4��0  �4�x�0 �5(p3,p2',p1)
   �25.�0 ��7l�0y.yx�    ��+�0 �7l�0y.�yx�  �4     ��+�4     �7l�0y.(yN), �᫨ y�7��0FV(N).
      �4��0     �4��5N�0        �4��0  �4��5N

      �4��0     �4�x�0 �5(p6)�0   �4���0  �4�y�x��5(p3,p1,p2')�0  �4��0  �4�x�0 �5(p3,p2',p1)
   �26.�0 ��7l�0y.yx�   ��+�0  �7l�0z.��yx� � � �4    ��+�4    �7l�0z.�zx� �4      ��+
      �4��0     �4��5N�0        �4���0  �4��5z�4��5N�4��0    �4         ��0  �4��5N

                ��+�7l�0z.(zN), �᫨ y�7��0FV(N).

      �4��0         �4�x�0     �4��0      �4�x
   �27.�0 ��7l�0y.�7l�0x.fxy�  ��+�7l�0z.��7l�0x.fxz�  ��+�7l�0z.�7l�0x.fxz.
      �4��0         �4��52y�0    �4��0      �4��52y

��1.2
   � �裡  �  �ਢ���� ��� ��।������� १���� ����⠭����
�7l�0-�ଠ �1N�0 ����� ��� ᢮������ �宦����� �।��⭮� ��६����� x �
�7l�0-�� M, ����� (!) �ਭ��� ᫥���饥
   �_�1�����襭�� ��७�ॣ��0 �1�� ������ ��६�����
                 (�1Barendregt convention�0) [��७�ॣ�,1985,�.38-39].
��1.1
   �᫨ � ��।��񭭮� ��⥬���᪮� ���⥪�� (��।������, ����-
��⥫��⢥) ��������� �7l�0-����4,�0 � ���ࠧ㬥������, ��:
   (1) �1�易���  ��६����  �  ��� ��࠭� �⫨�묨 �� ᢮������
�1��६������0;
   (2) ����� �易���� ��६����� ������ ���� ࠧ���묨.

��1.2
   �ᯮ���� �� ᮣ��襭��,  ����� ࠡ���� � �7l�0-�ଠ��  "�������2"
��ࠧ��, �.�. �믮����� ������ ����⠭���� ��� �7l�0-�ଠ��, �� ��-
����� ����稥 �������� ��६����� �� ����⠭����.
   �2�ਬ���0.
��1.1
   ����� �ਭ�������  ᮣ��襭�� ��७�ॣ� � ����室��� �믮�����
������ "�1����⠭�����0"
��1.0

   �4��0    �4�x
   ��7l�0y.M� .
   �4��0    �4��5N

��1.1
   ����� �祢����, �� y�7��0FV(N) � �ਬ������ �1����⠭���� ��� �����-
�1��� ��६������0.
��1.0

>  -- �����襭�� ��७�ॣ� ����� ������஢��� ⠪:
>  -- abc x x = x

   ERROR abc.hs  - Repeated variable "x" in pattern

   -- ���������� �㭪樨, ॠ�����饩 ������ "����⠭����
   -- �ﬡ��-�ଠ n ����� ��� ᢮������ �宦����� �।���-
   -- ��� ��६����� � � �ﬡ��-��".

>  podstanovka :: Lambda -> String -> Lambda -> Lambda
>  podstanovka (Var p) x lam | x==p = lam
>                            | True = Var p
>  podstanovka (App m1 m2) x lam = App (podstanovka m1 x lam)
>                                      (podstanovka m2 x lam)
>  podstanovka (Lam p m) x lam
>          | x==p = Lam p m
>          | p `inside` lam
>                 = Lam (p ++ x) (podstanovka
>                                   (podstanovka m p (Var (p ++ x)))
>                                   x
>                                   lam)
>          | True = Lam p (podstanovka m x lam)
>       where inside :: String -> Lambda -> Bool
>             s `inside` (Var x)   = x==s
>             s `inside` (App m n) = s `inside` m || s `inside` n
>             s `inside` (Lam _ n) = s `inside` n

>  t1 = Lam "x" (App (Var "x") (Var "y"))   -- \x.xy
>  t2 = Lam "x" (App (Var "x") (Lam "y" (App (Var "y") (Var "z"))))
>                                           -- \x.x(\y.yz)
>  t3 = Lam "x" (App (Var "x") 
>                    (App (Var "y") 
>                         (App (Var "z")
>                              (Lam "y" (App (Var "y") 
>                                            (Var "x"))))))
>                                           -- \x.x(y(z(\y.yx)))
> --------------------------------------------------------------
>  test6 =  podstanovka t1 "x" (Var "y") == t1
>        && podstanovka t1 "y" (Var "z") ==
>                            Lam "x" (App (Var "x") (Var "z"))

>        && podstanovka t1 "y" (Var "x") ==
>                            Lam "xy" (App (Var "xy") (Var "x"))
>        && podstanovka t2 "z" (Var "k") ==
>                            Lam "x" (App (Var "x")
>                                         (Lam "y"
>                                             (App (Var "y")
>                                                  (Var "k"))))
>        && podstanovka t3 "y" (Var "x") ==
>               Lam "xy" (App (Var "xy")
>                             (App (Var "x")
>                                  (App (Var "z")
>                                       (Lam "y"
>                                          (App (Var "y")
>                                               (Var "xy"))))))
>        && podstanovka t3 "x" (Var "y") == t3
>        && podstanovka t3 "z" (Var "k") ==
>               Lam "x" (App (Var "x")
>                            (App (Var "y")
>                                 (App (Var "k")
>                                      (Lam "y" (App (Var "y")
>                                                    (Var "x"))))))
>        && podstanovka t3 "z" (Var "y") ==
>               Lam "x" (App (Var "x")
>                            (App (Var "y")
>                                 (App (Var "y")
>                                      (Lam "yz" (App (Var "yz")
>                                                     (Var "x"))))))
>  test7 =   podstanovka (Lam "x" (Var "x")) "y" (Var "z")
>                 == Lam "x" (Var "x")
>         && podstanovka
>               (Lam "y" (App (Var "z") (App (Var "x") (Var "y"))))
>               "x"
>               (Lam "x" (Var "x"))
>                 == Lam "y" (App (Var "z")
>                                 (App (Lam "x" (Var "x"))
>                                      (Var "y")))
>         && podstanovka
>               (Lam "x" (App (Var "y") (Var "x")))
>               "x"
>               (App (Var "v") (Var "x"))
>                 == Lam "x" (App (Var "y") (Var "x"))
>         && podstanovka
>               (Lam "y" (App (Var "y") (Var "x")))
>               "x"
>               (App (Var "v") (Var "x"))
>                 == (Lam "y" (App (Var "y")
>                             (App (Var "v") (Var "x"))))

��1.2
                 �24.�0 �3�।�⠢����� �� ������樨
                             �7l�3-�ମ�
   ��������, ��  ���  ������� �㭪樨 �����筮 㪠���� ������⢠
X, Y � �1�ࠢ��� ᮯ��⠢������0, ���஥ �����뢠���� � ����:
��1.0

   x���76�0f(x),

��1.2
� ��  ���஬�  �����  x�7��0X  ��ꥤ������ � ���� � ������� y�7��0Y (�
��⭮��, �� ���� ����� ���� ���� ����᫥��).
   �2��।�������0.
��1.1
   �_�1����⨪�-������⢥���� ������樥��.�0 �7l�1-�_�ଠ
��1.0

   �7l�0x.�2(�0fx�2)

���뢠���� �1�ࠢ��� ᮯ��⠢������0 x���76�0f(x).

��1.2
   �ਢ��� �1ᮤ�ঠ⥫���  ⥮�⨪�-������⢥�����0   �1��������
�7l�0-�ମ� � ������� ��᪮�쪨� �ਬ�஢:
��1.1
   (1) (�7l�0x.x) ����������� ⮦���⢥���� �㭪樥� y=x � ����-
����, ࠢ�� �� ��㬥���;
   (2) (�7l�0x.f(gx)) ����������� �������樥� �㭪権 f � g,  �.�.
�㭪樥� � ���祭���, ࠢ�� f(g(x)) ��� ��㬥�� x;
   (3) (�7l�0x.(fg)x) ����������� �ਬ������� �1�㭪樮�����0 f � ���
��㬥�⠬: �㭪樮���쭮��  - g � "���筮��" - x.  ��� ᯮᮡ ��-
����樨 �������� ����뢠�� �1�㭪樨 ������0 �1���浪���0;
   (4) (�7l�0f.�7l�0x.fx) ����������� �㭪樥� ���襣� ���浪�, ����-
���� ���ன ��� �㭪樨 f � ��㬥�� x ���� ���祭�� f(x).

��1.2
   �3����砭���0.
��1.1
   � C++11 ����祭� �����প��7 l�0-�㭪権. �� �������� �㭪樨, ��-
���  �ᯮ�������  ⮫쪮  ���� ࠧ (��� � ����⢥ ��㬥�⮢)
��㣮� �㭪樨. ���ਬ��,
��1.0

   int main()
   {
     int a=fun([=] (int x) { return x*x*x; });
     ...
   }

��1.1
   ���⠪���7 l�0-�㭪権 �������祭 ᨭ⠪��� �� ��㣮�  �㭪樨,
⮫쪮 ���� ����� �������� ��ࠦ���� [=], � ⨯ �����頥���� ���-
祭�� ���筮 �������� �� ���� - ���������  �뢥���  ���  ᠬ����-
⥫쭮.

��1.2
              �25.�0 �3������� � ᮤ�ঠ⥫쭮� ��⥬�⨪�
                      �3��0 �3��⥬���᪮� ������
   ����㤭� ������,  �� ᫮��  �7l�0x  �����  ⠪��  ��  "��뢠�騥"
᢮��⢠, �� � ᫮�� �7��0x � �몥 �ଠ�쭮� ��⥬� ��ࢮ�� ���浪�
����7 3�0f(x)dx � ��⥬���᪮� �������.
   � �裡 �  �⨬  �।�⠢���  �����  ����᫥���  �1�����஢�0,
����������  � �몠�  ᮤ�ঠ⥫쭮� ��⥬�⨪� � ��⥬���᪮�
������ (�� [���������,1997,�.66,158; 2000,�.46,145]):
   (1) �_�1������ ��ࠧ������ ������⢠�.�0:
��1.0

   {x�P(x)},

��1.2
��� P(x)  -  �।����.  ��� ������ ���������� ������⢮ ��� x,
��������� ᢮��⢮� P(x);
   (2) ����樨 �_�1�ந���������.�0, �_�1�㬬��.�0, �_�1���᫥��� �।����.�0, �_�1���᫥���
�_�1��⥣ࠫ��.�0, �_�1��ꥤ������ ��������.�0:
��1.0

    n          n                   y
   ���        �7���0                   �7!�0          �7$
   � �(x�52�0+y),  �7��0(x�52�0+y), �1lim�0(x�52�0+y), �72�0(x�52�0+z)dx, �Ͱ�0 A�4k�0;
   � �        �7���0        x�76�01�5       �0 �71�0         �4k=1
   x=0        x=0                  0

��1.2
   (3) �_�1������ ��������樨
��1.0

   (�7m�0x�7��0X)P(x),

��1.2
��������騩 �������襥 �᫮ � ������⢥ ����ࠫ��� �ᥫ X,  ��-
�����饥 ����� ᢮��⢮� P(x).  ���  ������  ������  �  ᨫ�
�1�ਭ樯�  �������襣�  �᫠�0 (��᪮��� �� �������襥 �᫮ ���� �
⮫쪮 ���� � ������ �����⮬ ������⢥ ����ࠫ��� �ᥫ);
   (4) �_�1������ "�� ᠬ��.�0 �1x"�0:
��1.0

   �7i�0xP(x),

��1.2
��� P(x) - ��㫠 �몠 ��ࢮ�� ���浪�,  ����� ������ �ਭ�����
���祭�� "�1��⨭��0" ��� �����⢥����� x, ���஥ � ���� ���祭���
�ଠ �7i�0xP(x);
   (5) �_�1������ �㭪樮���쭮���.�0:
��1.0

   �7l�0x.t(x), ���� x���76�0t(x),

��1.2
��� t(x) - ��.  ��� ������ ���������� �� ��� t(x)  �㭪��,
���������  ���祭�� �⮣� �ଠ.  ������ ��� ����砥��� � ��-
�ଠ⨪�; � �ணࠬ��஢���� ���  ᮮ⢥�����  ���ᠭ��  �㭪樨
(t - ⥫� �㭪樨, x - �� �ଠ��� ��ࠬ���).
                      �3�������᪨� ᢥ�����
��1.1
   �_�1���� �������.�0 (14.06.1903-11.08.1995) - ���ਪ��᪨� ����� � ���-
��⨪, 童� ���ਪ��᪮� �������� ������.
   ������� � ��設�⮭�. ����稫 �ਭ�⮭᪨� 㭨������ (1927).
   � 1927-1928 �� �⨯�����⮬ �� ��⥬�⨪� � ��ࢠ�᪮� 㭨���-
���, � 1928-1929 ��蠫 ���樨 � ���⨭���᪮�,  � 1929 - � ���-
�ठ�᪮� 㭨�������. � 1929 ࠡ�⠫ � �ਭ�⮭᪮� 㭨�����-
� (� 1939 - ������ ��⥬�⨪�, � 1961 - ������ 䨫��䨨), �
1967 - ������ �����୨�᪮�� 㭨������ � ���-�������.
   �᭮��� ��᫥������� �⭮����� � ��⥬���᪮� ������.  ����-
��⠫ (1932-1933) ��⥬� ��ᨮ� ��� ��饩 ⥮ਨ ������ � ��� ���-
��� ��㫨஢�� ⥮ਨ,  ������� ����� ���᫥����  �7l�0-�८�ࠧ���-
���.  ��᫥�����  (1936)  �����  ���᫨��� �㭪権,  �뤥��� �����
�7l�0-��।������ �㭪権.  � ⮬ �� ���� ���㫨஢�� �1⥧�� �����0,  �
���஬ �⢥ত�����, �� ��� ������ �।���⮢ ����� ���� ������-
�� ��饣� ��⮤� �襭�� ����� �⭮�⥫쭮 ��饧��稬���  ����-
�ன ����. �� ࠡ�� �� ��⥬���᪮� � ᨬ�����᪮� ������.
��1.2

            �2������� ������� ��������� ����� ����������
   �2�ਬ�� 1.�0  ������ ������⢠ �易���� � ᢮������ ��६�����  �
�7l�0-�ଥ
��1.0

   �2(�7l�0x.xy�2)(�7l�0y.y�2)�0.

��1.2
   �_�1��襭���.�0. ��ᯮ��㥬�� ����⨢�묨 ��।�����ﬨ �㭪権 FV() �
CV():
��1.0
     �4��0             �4�
   FV�(�7l�0x.xy)(�7l�0y.y)���+�0FV(�7l�0x.xy)�7u�0FV(�7l�0y.y)��+�0(FV(xy)\{x})�7u�0(FV(y)\{y})��+
     �4��0             �4�
                    ��+�0((FV(x)�7u�0FV(y))\{x})�7u�0({y}\{y})��+

                    ��+�0({x}�7u�0{y})\{x})�7u'��+�0{y};
     �4��0             �4�
   CV�(�7l�0x.xy)(�7l�0y.y)���+�0CV(�7l�0x.xy)�7u�0CV(�7l�0y.y)��+�0{x}�7u�0CV(xy)�7u�0{y}�7u�0CV(y)��+
     �4��0             �4�
                    ��+�0{x,y}�7u�0CV(x)�7u�0CV(y)��+�0{x,y}.
��1.2

                     �2���������������� �������
   �2�ਬ�� 0.
��1.0

��1.2

   �2�ਬ�� 1.
��1.0

��1.2

              �2���������� ��� ���������������� �������
               �21.�0 �3����0 �7l�0-�3�ମ��0 �3��� �� � ��䠢��
   �21.�0 �������, �� ᫥���騥 ᫮�� ����� �7l�0-�ଠ��:
��1.0

   (�) (x�41�0x�41�0);            (�) (x�41�0(�7l�0x�41�0.(�7l�0x�42�0.x�43�0)));

   (�) (�7l�0x�42�0.x�42�0);          (�) ((x�42�0x�41�0)(�7l�0x�41�0.(�7l�0x�41�0.x�41�0)));

   (�) (�7l�0x�41�0.(x�41�0x�41�0));      (�) ((�7l�0x�42�0.x�42�0)(�7l�0x�43�0.(x�43�0x�42�0)));

   (�) (�7l�0x�41�0.(x�41�0(x�42�0x�42�0)));  (�) (�7l�0x�41�0.(�7l�0x�42�0.((x�42�0x�41�0)(�7l�0x�43�0.x�43�0)))).

��1.2
   �_�1��襭���.�0.
��1.0

   (�)      (�)        (�)            (�)

   x�41�0  x�41�0   x�42�0    x�42�0         x�41�0  x�41�0             x�42�0  x�42
   ������   ��������         ������             ������
   (x�41�0x�41�0)   (�7l�0x�42�0.x�42�0)   x�41�0    (x�41�0x�41�0)         x�41�0  (x�42�0x�42�0)
                       ������������         ����������
                       (�7l�0x�41�0.(x�41�0x�41�0))   x�41�0    (x�41�0(x�42�0x�42�0))
                                      ����������������
                                      (�7l�0x�41�0.(x�41�0(x�42�0x�42�0)))

��1.2
   �22.�0 �������, �� ᫥���騥 ᫮�� �� ����� �7l�0-�ଠ��:
��1.0

   (�) ((��42�0)(��41�0��43�0));          (�) ((��43�0��42�0x�43�0)(�7l�0x�41�0.(�7l�0x�41�0.��41�0)));

   (�) (�7l�0x�42�0.�7l�0x�42�0.(��42�0��42�0));      (�) ((��41�0��42�0).(�7l�0x�41�0.��41�0));

   (�) ((�7l�0x�43�0.��43�0��42�0)(�7l�0x�42�0.��42�0));  (�) (�7l�0x�41�0.(�7l�0��42�0((��42�0x�41�0)(�7l�0��43�0.��43�0)))).

��1.2
   �_�1��襭���.�0.
��1.0

   (�)           (�)                 (�)
   �4�� �����0       �4�����������0      �4����������
   �7��4-�ମ��0          �4���譨� ᪮����0   �4���譨� ᪮���

     �7^�0                     �7^�0               �7^
   (��42�0)  (��41�0��43�0)  x�42    �7l�0x�42�0.(��42�0��42�0)    x�43   �0��43�0��42
   ������������  �4�����������������0    �4����������
   ((��42�0)(��41�0��43�0))  (�7l�0x�42�0.�7l�0x�42�0.(��42�0��42�0))    (�7l�0x�43�0.��43�0��42�0)�4  �0(�7l�0x�42�0.��42�0)
                                     �4��������������������
                                     ((�7l�0x�43�0.��43�0��42�0)(�7l�0x�42�0.��42�0))

��1.2
   �23.�0 ������ ᪮��� � ᫥����� �7l�0-�ଠ�,  �������� �1ᮣ��襭�ﬨ
�1�� ������祭����0:
��1.0

   (�) ((�7l�0x�41�0.(�7l�0x�42�0.(��42�0��41�0)))(�7l�0x�43�0.��41�0));

   (�) ((��41�0��42�0)(�7l�0x�43�0.(��43�0(��44�0��42�0))));

   (�) (�7l�0x�43�0.((�7l�0x�41�0.((�7l�0x�42�0.��42�0)��43�0))��46�0));

   (�) ((�7l�0x�45�0.(��41�0��41�0))(�7l�0x�43�0.(�7l�0x�44�0.��44�0)));

   (�) (((��42�0��42�0)(��43�0(�7l�0x�41�0.��41�0)))(��44�0��43�0));

   (�) ((((��42�0(�7l�0x�43�0.��43�0))(�7l�0x�45�0.(�7l�0x�46�0.��45�0)))��41�0)��41�0).

�2���0�1.2
   �_�1��襭���.�0. ������,  ����� ����� �������,  �뤥���� ���� ���-
⮬.
�2���0�1.0

   (�) �2(�0(�7l�0x�41�0.�2(�7l�0x�42�0.�2(�0��42�0��41�2))�0)�2(�7l�0x�43�0.��41�2))�7=�0(�7l�0x�41�0x�42�0.��42�0��41�0)�7l�0x�43�0.��41�0;

   (�) �2((�0��41�0��42�2)�0(�7l�0x�43�0.�2(�0��43�0(��44�0��42�0)�2)�0)�2)�7=�0��41�0��42�0(�7l�0x�43�0.��43�0(��44�0��42�0));

   (�) �2(�7l�0x�43�0.(�2(�7l�0x�41�0.�2(�0(�7l�0x�42�0.��42�0)��43�2)�0)��46�2))�7=l�0x�43�0.(�7l�0x�41�0.(�7l�0x�42�0.��42�0)��43�0)��46�0.

��1.2
   �24.�0 ����⠭���� ᪮��� � ᫥����� �7l�0-�ଠ�,  �������� �1ᮣ���-
�1��ﬨ �� ������祭����0:
��1.0

   (�) (�7l�0x�41�0��42�0��43�0.��43�0��43�0)��42�0��44�0(��45�0��45�0);  (�) ��41�0��41�0(��43�0��43�0)��45�0(�7l�0x�46�0.��46�0);

   (�) (��42�0��43�7l�0x�42�0.��42�0)��45�0(�7l�0x�46�0.��46�0��47�0);  (�) (�7l�0x�42�0.��43�0��42�0)(��44�0��44�0)(�7l�0x�45�0.��46�0��45�0��44�0);

   (�) (�7l�0x�43�0��44�0.(��46�0��47�0)�7l�0x�48�0.��48�0);      (�) ��41�0(��42�7l�0x�43�0��45�0.��45�0)��46�0��48�0��49�0.

��1.2
   �_�1��襭���.�0. ����⠭������� ᪮��� �뤥���� ���� ���⮬.
��1.0

   (�) (�7l�0x�41�0��42�0��43�0.��43�0��43�0)��42�0��44�0(��45�0��45�0)�7=

                         �7=�2(((�0(�7l�0x�41�0.�2(�7l�0��42�0.�2(�7l�0��43�0.�2(�0��43�0��43�2)))�0)��42�2)�0��44�2)�0(��45�0��45�0)�2)�0;

   (�) (��42�0��43�7l�0x�42�0.��42�0)��45�0(�7l�0x�46�0.��46�0��47�0)�7=�2((�0(�2(�0��42�0��43�2)(�7l�0x�42�0.��42�2)�0)��45�2)�0(�7l�0x�46�0.�2(�0��46�0��47�2)�0)�2)�0;

   (�) (�7l�0x�43�0��44�0.(��46�0��47�0)�7l�0x�48�0.��48�0)�7=�0(�7l�0x�43�0.�2(�7l�0��44�0.�2(�0(��46�0��47�0)�2(�7l�0x�48�0.��48�2)))�0).

��1.2
                �22.�3 �������� � �易��� ��६����
   �21.�0 (�� [��७�ॣ�,1985,�.37])
   ����� M��+�7l�0x.xy�2(�7l�0z.y�2)�0. ��⠭���� ��୮ ��, ��
��1.0

   �2(�0xy�2)�7��1Sub�0(M), z�7��1Sub�0(M) �2(�0y�2(�7l�0z.y�2))�7��1Sub�0(M)?

��1.2
   �22.�0 ������ ������⢠ ᢮������ � �易���� ��६����� ��� ᫥��-
��� �7l�0-�ମ�:
��1.0

   (�) (�7l�0x.xy)(�7l�0y.y);        (�) y(�7l�0y.yx)(�7l�0x.x);

   (�) �7l�0x.�7l�0y.z(�7l�0z.z(�7l�0x.y));  (�) (�7l�0x.�7l�0y.xz(yz))(�7l�0x.y(�7l�0y.y));

   (�) (�7l�0y.xyz)(�7l�0z.�7l�0x.xy);   (�) (�7l�0x.�7l�0y.x(�7l�0z.yz))(�7l�0x.�7l�0y.z).

��1.2
   �_�1��襭���.�0.
��1.0

   (�) FV((�7l�0x.xy)(�7l�0y.y))��+�0FV(�7l�0x.xy)�7u�0FV(�7l�0y.y)��+

           ��+�0(FV(xy)\{x})�7u�0(FV(y)\{y})��+�0((FV(x)�7u�0FV(y))\{x})�7u�0({y}\{y})��+

           ��+�0({x}�7u�0{y})\{x})�7u'��+�0{y};

       CV((�7l�0x.xy)(�7l�0y.y))��+�0CV(�7l�0x.xy)�7u�0CV(�7l�0y.y)��+�0{x}�7u�0CV(xy)�7u�0{y}�7u�0CV(y)��+

           ��+�0{x,y}�7u�0CV(x)�7u�0CV(y)��+�0{x,y}.

��1.2
                     �23.�0 �3�������0 �3"����⠭����"
                          �3��2 �7l�2-�3���᫥���
   �21�4*�2.�0 �믮���� ������ "����⠭����":
��1.0

       �4��0     �4�x�0        �4��0          �4�x
   (�)�2 �0��7l�0y.yx�  ;  (�) �(�7l�0z.x)(xy)� ;
       �4��0     �4��5zx�0 �5  �0    �4��0          �4��5z
       �4��0     �4�x�0        �4��0        �4�x
   (�)�2 �0��7l�0y.yx�  ;  (�) ��7l�0y.z(xy)�    .
       �4��0     �4��5yx�0       �4��0        �4��7��5x.x

��1.2
   �_�1��襭���.�0.
��1.0

       �7(�0     �7)�4x   (p5)�0   �7(�0  �7)�4x  (p3,p2',p1)
   �2(�) �0��7l�0y.yx�72�0    �2 ��+�4  �7l�0y.�yx�72�0     �4 �0  ��+�0   �4  �7l�0y.y(zx);
       �79�0     �70�5(zx)�0       �79�0  �70�5(zx)

��1.2
   �22�4*�2.�0 �믮���� ������ "�1����⠭�����0":
��1.0

       �4��0          �4�x�0       �5 �0   �4��0       �4�x
   (�)�2 �0��7l�0x.�7l�0y.�7l�0z.x�     ; �5 �0(�) ��7l�0z.�7l�0x.x� ;
       �4��0          �4��7��5x.zx�0    �5 �0  �4��0       �4��5y
       �4��0        �4�x�0             �4��0         �4�x
   (�)�2 �0��7l�0y.�7l�0z.yx�    ;     (�) ��7l�0z.�7l�0y.xyz�     .
       �4��0        �4��7��5y.x �0         �4��0         �4��7��5y.xz

��1.2
   �_�1��襭���.�0.
��1.0

       �7(�0          �7)�4x�0     �4(p4)
   �2(�) �0��7l�0x.�7l�0y.�7l�0z.x�72�0       ��+�7l�0x.�7l�0y.�7l�0z.x;
       �79�0          �70�5(�7��5x.zx)

��1.2
                �24.�3 �������୮� ������஢�����0 �3�몠
                 �7l�3-���᫥��� ��� �몠 � ��䠢��
   �21.�0 �஢���� ���஢���� �ணࠬ� �� ࠧ����  "��������樮���
�ਬ���" �� ����祭�� १���⠬ �襭�� �����.
   �22.�0 ���⠢�� �ணࠬ�� ��� ����஥��� �ॢ�᭮��  ������⥫��⢠
⮣�, �� ������ ᫮�� ���� �7l�0-�ମ�.
   �23.�0 ���⠢�� �ணࠬ�� ��� ��।������ ࠭�� �7l�0-�ଠ.
   �24.�0 ���⠢��  �ணࠬ�� ��� ��।������ ⮣� 䠪�,  ����� ��
��� �ଠ �����묨.
   �25.�0 ���⠢�� �ணࠬ��, ॠ�������� ������ "�1����⠭���� �7l�1-��-
�1��  N  �����  ���  ᢮������  �宦����� �।��⭮� ��६����� � �
�7l�1-�� M�0".
   �26.�0 ���⠢�� �ணࠬ��-�࠭����, ��ॢ���騩:
   (1) ᮪����� ������ �7l�0-�ଠ � ������ �7l�0-�ଠ � �ᥬ� ᪮���-
�� � ����ࠪ�ﬨ �� ᮣ��襭�� �� ������祭��� �7l�0-�ମ�;
   (2) �7l�0-��  � ᮪����� ������,  ���᪠� ᪮��� � ���ࠪ樨 ��
ᮣ��襭�� �� ������祭��� �7l�0-�ମ�.
   �27.�0 ���⠢��  �ணࠬ��,  ������������ ������ �㭪樮���쭮��,
����������饣� �� ��� �㭪��, ��������� ���祭�� �⮣� �ଠ.

                      �3�ᯮ����⥫�� �㭪樨

>  instance Show Lambda where
>     show (Var x)   = x
>     show (App x y) = case y of
>                        App _ _ -> showLam x ++ "(" ++ show y ++ ")"
>                        _       -> showLam x ++ showLam y
>           where showLam l@(Lam _ _) = "(" ++  show l  ++ ")"
>                 showLam x           = show x
>     show (Lam x e)  = "\\" ++ x ++ "." ++ show e

>  povto []  = []
>  povto [x] = [x]
>  povto (x:y:lst) | x==y = povto1 [x] []  lst
>                  | True = povto1 [x] [y] lst

>  povto1 lst1      []       []    = reverse lst1
>  povto1 lst1    (y:lst2)   []    = povto1 ([y] ++ lst1) [] lst2
>  povto1 (x:lst1) lst2   (y:lst3)
>                   | x==y = povto1 (x:lst1) lst2          lst3
>                   | True = povto1 (x:lst1) (lst2 ++ [y]) lst3

>  test8 = povto [1,2,3,4,1,2,3,4]
>  test9 = povto [1,2,3,4]
