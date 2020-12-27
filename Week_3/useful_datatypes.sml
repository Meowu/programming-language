

datatype exp = CONSTANTS of int
             | NEGATE of exp
             | ADD of exp * exp
             | MULTIPLY of exp * exp


fun eval(x) =
  case x of
       CONSTANTS i      => i
     | NEGATE e         => ~ (eval e)
     | ADD(e1, e2)      => (eval e1) + (eval e2)
     | MULTIPLY(e1, e2) => (eval e1) * (eval e2)

val a = ADD(CONSTANTS 2, CONSTANTS 3);
val b = ADD(NEGATE (CONSTANTS 2), NEGATE (CONSTANTS ~2));


fun number_of_adds e = 
  case e of
       CONSTANTS i      => i
     | NEGATE e1        => ~ (number_of_adds e1)
     | ADD(e1, e2)      => (number_of_adds e1) + (number_of_adds e2)
     | MULTIPLY(e1, e2) => (number_of_adds e1) * (number_of_adds e2)

val c = number_of_adds (CONSTANTS 3)
val d = number_of_adds a

