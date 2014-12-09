signature STACK =
sig
    type 'a Stack
    val empty : 'a Stack
    val isEmpty : 'a Stack -> bool
    val cons : 'a * 'a Stack -> 'a Stack
    val head : 'a Stack -> 'a
    val tail : 'a Stack -> 'a Stack                                                 
end
    

structure List : STACK =
struct
  type 'a Stack = 'a list
  val empty = []
  fun isEmpty xs = null xs
  fun cons (x,xs) = x::xs
  fun head xs = hd xs
  fun tail xs = tl xs
end


structure CustomStack : STACK =
struct
  datatype 'a Stack = NIL | CONS of 'a * 'a Stack
  exception EMPTY
  val empty = NIL
  fun isEmpty NIL = true
    | isEmpty _  = false
  fun cons (x, xs) = CONS (x,xs)
  fun head NIL = raise EMPTY
    | head (CONS (x,xs)) = x
  fun tail NIL = raise EMPTY
    | tail (CONS (x, xs)) = xs
end
