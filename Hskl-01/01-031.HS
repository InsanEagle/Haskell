   -- Демонстрация синтаксиса и семантики условного выражения,
   -- организующего множественное ветвление.
   -----------------------------------------
   -- Укажите назначение приведённых функций
   -----------------------------------------
   getDay:: Integer -> [Char]
   getDay day = case (day) of 
                  (1) -> "Monday"
                  (2) -> "Tuesday"        
                  (3) -> "Wednesday"
                  (4) -> "Thursday"
                  (5) -> "Friday"
                  (6) -> "Saturday"		   
                  (7) -> "Sunday"		   
                  (_) -> "No such day of week"
   -------------------------------------------
   getDate:: [Char] -> Integer
   getDate date = case (date) of 
                    ("Monday")    -> 1
                    ("Tuesday")   -> 2        
                    ("Wednesday") -> 3
                    ("Thursday")  -> 4
                    ("Friday")    -> 5
                    ("Saturday")  -> 6
                    ("Sunday")    -> 7
   ------------------------------------		   
   defDay:: Integer -> [Char] -> [Char]
   defDay d dow | d<1 || d>31  = error "No such day in calendar"
         	| d `mod` 7==1 = dow
		| d>1 && d<=7  = getDay((getDate(dow)+d-1)`mod`7)
		| otherwise    = getDay(getDate(dow)`mod`7-1)
   ----------------------------------------------------------
   -- Неудачные тестовые примеры:
   --------------------------------------
   test =   defDay 1 "Monday" == "Monday" 
         && defDay 2 "Monday" == "Tuesday"
         && defDay 4 "Sunday" == "Wednesday"
