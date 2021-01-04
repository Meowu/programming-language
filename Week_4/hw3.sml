
(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs = List.foldl (fn (x, acc) => if String.size x >
  String.size acc
                                                    then x else acc) "" xs

fun longest_string2 xs = List.foldl (fn (x, acc) => if String.size x >=
  String.size acc
                                                    then x else acc) "" xs

fun longest_string_helper f xs = List.foldl (fn (x, acc) => if f(String.size x,
  String.size acc) then x else acc) "" xs

val longest_string3 = fn xs => longest_string_helper (fn (s1, s2) => s1 > s2) xs

val longest_string4 = fn xs => longest_string_helper (fn (s1, s2) => s1 >= s2) xs

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs = 
  let fun find_answer(ls) = 
        case ls of
             [] => raise NoAnswer
           | x::xs' => case f(x) of
                            NONE => find_answer(xs')
                          | SOME v => v
  in
    find_answer(xs)
  end

fun all_answers f xs = 
  let 
    fun helper ls acc =
      case ls of
           [] => acc
         | x::ls' => case f(x) of
                         NONE => helper ls' acc
                       | SOME v => helper ls' (v @ acc)
    val result = helper xs []
  in
    if List.length result = List.length xs then SOME result else NONE
  end

fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
  let 
    fun helper p acc = 
      case p of
           Wildcard => acc
         | Variable x => x::acc
         | TupleP ps => List.foldl (fn (p, acc) => helper p acc) acc ps
         | ConstructorP(_, p) => helper p acc
         | _ => acc
    fun repeats vars =
      case vars of
           [] => true
         | var::vars' => not (List.exists (fn x => x = var) vars') andalso
         (repeats vars')

    val vars = helper p []
  in
    repeats(vars)
  end 

(*
(* Q11 *)
fun match (v,p) =
    case (p,v) of
        (Wildcard, _) => SOME []
      | (Variable s, v) => SOME [(s, v)]
      | (UnitP, Unit) => SOME []
      | (ConstP i', Const i) => if i' = i then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs
				 then all_answers (match) (ListPair.zip (vs,ps))
				 else NONE 
      | (ConstructorP(s1,p'), Constructor(s2,q)) => if s1 = s2 then match (q,p') else NONE 
      | _ => NONE

* *)

fun match(vs, ps) =
  case ps of
       Wildcard => SOME []
     | Variable s => SOME [(s, vs)]
     | UnitP => (case vs of
                     Unit => SOME []
                   | _ => NONE)
     | ConstP v => (case vs of
                        Const(n) => if v = n 
                                   then SOME []
                                   else NONE
                      | _ => NONE)
     | TupleP ps' => (case vs of
                          Tuple(vs') => if List.length ps' = List.length vs' 
                                        then all_answers match (ListPair.zip(vs', ps'))
                                       else NONE
                        | _ => NONE)
     | ConstructorP(s1, p) => (case vs of
                                   Constructor(s2, v) => if s1 = s2
                                                        then match(v, p)
                                                        else NONE
                                 | _ => NONE)

fun first_match v ps = SOME(first_answer (fn p => match(v, p)) ps) handle
  NoAnswer => NONE
