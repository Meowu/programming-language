
fun map(f, xs) =
  case xs of
       [] => []
     | x::xs' => (f x)::map(f, xs')

fun filter(f, xs) =
  case xs of
       [] => []
     | x::xs' => if f x
                 then x::(filter (f, xs'))
                 else filter(f, xs)

fun is_even v = 
  (v mod 2 = 0)


