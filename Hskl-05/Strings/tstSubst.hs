   -- Тестовые примеры функций библиотеки Subst.hs
   -- по работе с операциями над строками
   --------------------------------------
   import Subst
   ---------------------------------------------------------
   test = testQuan1  && testBeg && testFin  && testNoInt1 &&
          testFirst  && testEnd && testQuan && testSubst1 && 
          testSubst2 && tetInt3 && testInt1 && testInt2
   ----------------------------------------------------
   testQuan1 =   quan1 "ab" "abbaab"  == 2
              && quan1 "aa" "aaa"     == 1
              && quan1 "aba" ""       == 0
              && quan1 "aba" "babbab" == 0
              && quan1 "q"   "qqqqq"  == 5
   ---------------------------------------
   testBeg =   beg "abc" "a"      == False
            && beg ""    "abc"    == True
            && beg "abc" "abccba" == True
            && beg "abc" "abbabc" == False
            && beg "abc" "abb"    == False
   ---------------------------------------
   testFin =   fin "abc" "a"      == False
            && fin ""    "abc"    == True
            && fin "abc" "bcaabc" == True
            && fin "abc" "abcabb" == False
            && fin "abc" "abb"    == False
   --------------------------------------------
   testNoInt1 =   noInt1 "ab"  ""       == True
               && noInt1 "ab"  "abba"   == False
               && noInt1 "ab"  "baabba" == False
               && noInt1 "ab"  "bbbab"  == False
               && noInt1 "aba" "abbab"  == True
   --------------------------------------------
   testFirst =   first "ab" "abbaab" == ""
              && first "ab" "bbabbb" == "bb"
              && first ""   "aaa"    == ""
              && first "aa" "baaab"  == "b"
              && first "ab" "ab"     == ""
   ---------------------------------------
   testEnd =   end "ab"   ""   == "ab"
            && end "abc"  "ab" == "c"
            && end "aaaa" "a"  == "aaa"
            && end "a"    "a"  == ""
   -----------------------------------
   testQuan =   quan "ab" ""      == 0
             && quan "aa" "bb"    == 0
             && quan "ab" "baab"  == 1
             && quan "a"  "aaa"   == 3
             && quan "aa" "aaaaa" == 4
   -----------------------------------------------
   testSubst1 =   subst1 ""       "a"   "b"  == ""
               && subst1 "abc"    "ba"  "cc" == "abc"
               && subst1 "cab"    "ab"  "c"  == "cc"
               && subst1 "ababa"  "aba" "q"  == "qba"
               && subst1 "abaaba" "aba" "q"  == "qq"
   ----------------------------------------------------
   testSubst2 =   subst2 ""         "a"   "b"   1 == ""
               && subst2 "abc"      "ba"  "cc"  1 == "abc"
               && subst2 "cba"      "ab"  "c"   2 == "cba"
               && subst2 "ababa"    "aba" "q"   2 == "abq"
               && subst2 "aaaaaaaa" "aa"  "bbb" 7 == "aaaaaabbb"
   -------------------------------------------------------------
   tetInt3 =   int3 1 "ab"  "abab"     == ""
            && int3 2 "ab"  "abcab"    == "abc"
            && int3 3 "aa"  "aaaa"     == "aa"
            && int3 2 "aba" "ababa"    == "ab"
            && int3 4 "a"   "bbbbaaaa" == "bbbbaaa"
   ------------------------------------------------
   testInt1 =   int1 1 'a' "aa"   == ""
             && int1 2 'a' "baa"  == "ba"
             && int1 3 'a' "aaaa" == "aa"
   --------------------------------------------
   testInt2 =   int2 1 "ab"  "abab"       == ""
             && int2 1 "ab"  "babcab"     == "b"
             && int2 2 "ab"  "abcab"      == "abc"
             && int2 3 "aba" "abababaaba" == "abababa"
             && int2 3 "aa"  "aaaaaa"     == "aaaa"
