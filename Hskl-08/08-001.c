   /* ����������:                                 */
   /*  (1) ����権, ����ண�� �� ��஬� ��㬥��� */
   /*      (�⫮������ ���᫥���);                 */
   /*  (2) ���ࣨ��� ���᫥���                    */
   /* --------------------------------------------- */
   #include<stdio.h>
   #include<conio.h>
      int bot();
      int const_1 (int);
   /* --------------- */
   int main()
   {
      int x=0, y=1;
      /* ----------------------------- */
      /* ����� �⫮����� ���᫥��� */
      /* ----------------------------- */
      printf("(1) %d\n",y||5/x);
      getch();
      printf("(2) %d\n",0&&5/x);
      getch();
      printf("(3) %d\n",x<y?y: 1 / 0);
      getch();
      /* ---------------------------------- */
      /*  "��࠭��" �⫮����� ���᫥���  */
      /* ---------------------------------- */
      x=1 / 0; y=2; printf("(4) %d %d\n",x,y);
      getch();
      /* ------------------------------------------- */
      /*          "���ࣨ��" ���᫥���            */
      /* ------------------------------------------- */
      printf("��諨 � �㭪�� bot() � \"��ᨬ\"...\n");
      const_1 (bot());
      return 0;
   }
   /* --- */
   int bot()
   /* �㭪�� ॠ����� ��᪮����� ४���� */
   {
      return bot();
   }
   /* ------------- */
   int const_1 (int n)
   /* �㭪�� �����頥� 1 ������ᨬ� */
   /* �� ���祭�� ��㬥��           */
   {
      return 1;
   }