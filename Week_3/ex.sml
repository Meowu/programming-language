

datatype exp = CONSTANTS of int
             | NEGATE of exp
             | ADD of exp * exp
             | MULTIPLY of exp * exp


val a = ADD(CONSTANTS 2, CONSTANTS 3);
val b = ADD(NEGATE (CONSTANTS 2), NEGATE (CONSTANTS ~2));

fun max_nums e =
  case e of
       CONSTANTS i => i
     | NEGATE e1 => max_nums e1
     | ADD(e1, e2) => Int.max(max_nums e1, max_nums e2)
     | MULTIPLY(e1, e2) => Int.max(max_nums e1, max_nums e2)

val c = max_nums a;
val d = max_nums b;
