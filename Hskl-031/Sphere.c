   /* ���������� ���᫥��� ���頤� �����孮�� ��-  */
   /* ������� � ���� �������.                     */
   /* ------------------------------------------------ */
   /* ������⨢��� ⢮���⢮: 3-� ���� ��,            */
   /*                          28.09.2016, 12:15-13:00 */
   /* ------------------------------------------------ */
   #include<stdio.h>
   #include<math.h>
   #include<assert.h>
   #include<conio.h>
   #define EPS 0.0001
      double gamma (double);
      double s     (int);
      double v     (int);
   /* ---------------- */
   int main()
   {
      unsigned n;
      double y;
      // ----------------------------
      printf("������� (gamma):\n");
      for (double z=1.5; z<6.; z=z+0.5)
         printf("(%g,%g), ",z,gamma(z));
      printf("\n\n");
      getch();
      // ------------------------------------------
      printf("������� (���頤� �����孮��):\n");
      for (n=1; n<=8; n++)
         printf("(%u,%g), ",n,s(n));
      printf("\n\n");
      getch();
      // --------------------------------------
      printf("������� (���� �������):\n");
      for (n=2; n<=8; n++)
         printf("(%u,%g), ",n,v(n));
      printf("\n\n");
      getch();
      // ---------------------------------------------
      printf("������ ��㬥��   : "); scanf("%u",&n);
      printf("������� (���頤�): %g\n",s(n));
      printf("������� (����)  : %g\n",v(n));

      // ***************************
      // ��㤠�� ��⮢� �ਬ���:
      // -----------------------------------------
      assert(gamma(5./2.) ==3./4.   *sqrt (M_PI));
      assert(gamma(7./2.) ==15./8.  *sqrt (M_PI));
      assert(gamma(9./2.) ==105./16.*sqrt (M_PI));
      assert(gamma(11./2.)==945./32.*sqrt (M_PI));
      // --------------------------------------------
      assert(abs(s(1)-2.*M_PI)                 <EPS); 
      assert(abs(s(2)-4.*M_PI)                 <EPS);
      assert(abs(s(3)-2.*M_PI*M_PI)            <EPS);
      assert(abs(s(4)-8./3.*M_PI*M_PI)         <EPS);
      assert(abs(s(5)-M_PI*M_PI*M_PI)          <EPS);
      assert(abs(s(6)-16.0/15.0*M_PI*M_PI*M_PI)<EPS);
      // --------------------------------------------
      assert(abs(v(2)-M_PI)                   <EPS);
      assert(abs(v(3)-4./3.*M_PI)             <EPS);
      assert(abs(v(4)-M_PI*M_PI/2.)           <EPS);
      assert(abs(v(5)-8./15.*M_PI*M_PI)       <EPS);
      assert(abs(v(6)-M_PI*M_PI*M_PI/6.)      <EPS);
      assert(abs(v(7)-16./105.*M_PI*M_PI*M_PI)<EPS);
      // -------------------------------------------
      getch();
      return 0;
   }
   /* ----------------------------------------- */
   /* �㭪�� �����頥� ���祭�� gamma-�㭪樨 */
   /* ----------------------------------------- */
   double gamma (double x)
   {
      if (x==0.5)
        return sqrt (M_PI);
      else if (x==1)
             return 1;
           else return (x-1)*gamma (x-1);
   }
   /* -------------------------------------- */
   /* �㭪�� �����頥� ���頤� �����孮�� */
   /* ��������� �������� ࠧ��୮�� n      */
   /* -------------------------------------- */
   double s (int n)
   {
      double z=(n+1.)/2.;
      return 2.*pow(M_PI,z)/gamma(z);
   }
   /* ---------------------------------- */
   /* �㭪�� �����頥� ���� ������� */
   /* �������� ࠧ��୮�� n             */
   /* ---------------------------------- */
   double v (int n)
   {
      double z=n/2.0;
      return pow(M_PI,z)/gamma(1.+z);
   }
