
fun inc_or_zero int_option = 
  case int_option of
       NONE => 0
     | SOME i => i + 1


fun isNull x =
  case x of
       [] => true
     | x::x' => false


fun sum_list xs = 
  case xs of
       [] => 0
     | x :: xs' => x + sum_list(xs')


fun append (xs, ys) =
  case xs of
       [] => ys
     | x :: xs' => x :: append(xs', ys)


