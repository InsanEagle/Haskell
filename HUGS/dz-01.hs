  x1:: Integer; x1 = 14811
  x2:: Integer; x2 = -97531
  y1:: Double; y1 = 0.12345
  y2:: Double; y2 = -8765543.23
  z1:: Bool; z1 = True
  z2:: Bool; z2 = False
-------------------------------------------------------
--f1 = (x1-x2)/23  -- Int/Int - Error 
  f2 = x1 `div` x2
--f3 = x1+3.14-x2  -- Int+Double-Int - Error
  f4 = z1&&(y1>y2)||z2
--f5 = y2*x2-x1    -- Double*Int-Int - Error
  f6 = (y1+y2)>0&&(x1*x2)<0
--f7 = x2 `mod` 2 + y1 -- Int `mod` 2 + Double - Error

-------------------------------------------------------
 
--xxx s = s>='0' && s<='7' -- s от 0 до 7 
--yyy s = s>='0' && s<='9' -- s от 0 до 9
--zzz s = s>='A' && s<='F' || s>='a' && s<='f' -- s от 'A' до 'F' или от 'a' до 'f'
 
--func::Char -> Bool
  func a = a>='a' && a<='z' || a>='A' && a<='Z'
  
-------------------------------------------------------

  f x = if x<(-5) || x>5 
                     then abs (2*x)
		     else x*x-4

-------------------------------------------------------

  f8 x y = (x+10*y)/(x-y)

-------------------------------------------------------

  g x = case x of
      1 -> "January"
      2 -> "February"
      3 -> "March"
      4 -> "April"
      5 -> "May"
      6 -> "June"
      7 -> "July"
      8 -> "August"
      9 -> "September"
      10 -> "October"
      11 -> "November"
      12 -> "December"

-------------------------------------------------------

  s c = if c<1000
             then (c `div` 100) + (c `mod` 100 `div` 10) + (c `mod` 10) 
             else error "Too big number"

-------------------------------------------------------
   	 
  a x y z = if (z-y)==(y-x) && x/=y && y/=z && x/=z
	    then True
            else False                                     	




 