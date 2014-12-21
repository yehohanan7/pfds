fun even(x) = if x = 0 then true else
              if x > 0 then not(even(x-1))
              else not(even(x+1))

                      
