   -- Демонстрация проверки тождеств, связанных с
   -- функциями: 
   --   head, tail; init, last; take, drop
   -- **********************************************
   test lst@(x:xs) =   x:xs                   == lst
                    && head lst:xs            == lst
                    && x:tail lst             == lst
                    && head lst:tail lst      == lst
                    && take 1 lst++drop 1 lst == lst
                    --------------------------------
                    && init lst++[last lst]   == lst
                    && take (len-1) lst++
                       drop (len-1) lst       == lst
                    ---------------------------------------
                    && head lst:(init.tail) lst++[last lst] 
                                              == lst
                    --------------------------------------
                    && s((.)(++)(take len)) (drop len) lst
                                              == lst
        where len     = length lst
              s x y z = x z (y z)
                 
