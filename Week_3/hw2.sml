(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, xs) =
  let fun check(xs) = 
    case xs of
         [] => []
       | x::xs' => if same_string(s, x) then xs'
                  else x::check(xs')
    val result = check(xs)
  in
    if result = xs
    then NONE
    else SOME result
  end

fun get_substitutions1(xs, s) = 
  case xs of
       [] => []
     | x::xs' => case all_except_option(s, x) of
                      NONE => get_substitutions1(xs', s)
                    | SOME i => i @ get_substitutions1(xs', s)

fun get_substitutions2(xs, s) = 
  let fun helper(xs, acc) =
        case xs of
             [] => acc
           | x::xs' => case all_except_option(s, x) of
                            NONE => helper(xs', acc)
                          | SOME i => helper(xs', acc @ i)
  in
    helper(xs, [])
  end


fun similar_names(xs, {first=f, middle=m, last=l}) = 
  let fun replace_first(names) =
        case names of
             [] => []
           | n::ns' => { first = n, middle=m,
                       last=l}::replace_first(ns')
  in
       {first=f, middle=m, last=l}::replace_first(get_substitutions2(xs, f))
  end




(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color card = 
  case card of
       (Spades, _) => Black
     | (Clubs, _) => Black
     | _ => Red

fun card_value card = 
  case card of
       (_, Ace) => 11
     | (_, Num i) => i
     | _ => 10


fun remove_card(cs, c, e) = 
  let fun check(cs) =
        case cs of
             [] => []
           | x::cs' => if x = c then cs' else x::check(cs')
      val res = check(cs)
  in
    if res = cs
    then raise e
    else res
  end

fun all_same_color(xs) = 
  case xs of
       [] => true
     | _::[] => true
     | hd1::(hd2::xs') => card_color(hd1) = card_color(hd2) andalso
     all_same_color(hd2::xs')

fun sum_cards(xs) = 
  let fun tail_sum(xs, total) = 
        case xs of
             [] => total
           | x::xs' => tail_sum(xs', card_value(x) + total)
  in
    tail_sum(xs, 0)
  end



