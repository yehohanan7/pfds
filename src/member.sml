exception Nomatch;

fun member (x, []) = raise Nomatch
  | member (x, t::xs) = if x = t then t::xs
                        else member(x, xs);


member(2, [1,5,6]) handle Nomatch => (print("hello");[]);
