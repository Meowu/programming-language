
fun alternate(nums: int list) = 
  if null nums
  then 0
  else if null (tl nums)
  then hd nums
  else
    let 
      fun add(nums: int list, sign: string) =
        if null nums
        then 0
        else
          if sign = "+" (* must be double quote *)
          then (hd nums) + add(tl nums, "~")
          else ~(hd nums) + add(tl nums, "+")
    in
      add(nums, "+")
    end

fun min(xs: int list) = 
  if null xs
  then NONE
  else
    let 
      fun min_nonempty(xs: int list) =
        if null (tl xs)
        then hd xs
        else
          let val tl_ans = min_nonempty(tl xs)
          in
            if hd xs < tl_ans
            then hd xs
            else
              tl_ans
          end
    in
      SOME(min_nonempty xs)
    end

fun max(xs: int list) = 
  if null xs
  then NONE
  else
    let val tl_ans = max(tl xs)
    in
      if isSome(tl_ans) andalso valOf tl_ans > hd xs
      then tl_ans
      else SOME (hd xs)
    end

(* xs is non-empty list *)
fun min_max(xs: int list) = 
  let
    val min_value = min xs
    val max_value = max xs
  in
    (valOf min_value, valOf max_value)
  end