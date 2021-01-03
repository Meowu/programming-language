
fun f a b c = a + b + c

fun fold f acc xs =
  case xs of
       [] => acc
     | x::xs' => fold f (f(acc, x)) xs'

val sum = fn xs => fold (fn (acc, y) => acc + y) 0 xs


