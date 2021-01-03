

fun n_times(f, n, x) = 
  if n = 0
  then x
  else f(n_times(f, n - 1, x))

fun increment x = x + 1
fun double x = 2 * x
fun triple x = 3 * x

val a = n_times(increment, 4, 6)
val b = n_times(tl, 3, [12, 23, 43, 14, 2])
fun triple_times(n, x) = n_times(triple, n, x)

val c = triple_times(3, 3)

