
fun is_older (date1: int*int*int, date2: int*int*int) = 
  let 
    val year1 = #1 date1
    val year2 = #1 date2
    val month1 = #2 date1
    val month2 = #2 date2
  in
    if year1 = year2
    then
      if month1 = month2
      then
        (#3 date1) <  (#3 date2)
      else
        month1 < month2
    else year1 < year2
  end

fun number_in_month(dates: (int*int*int) list, month: int) = 
  if null dates
  then 0
  else if (#2 (hd dates)) = month
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)

fun number_in_months(dates: (int*int*int) list, months: int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) = 
  if null dates
  then []
  else
    let val date = hd dates;
    in
      if (#2 date) = month
      then
        date :: dates_in_month(tl dates, month)
      else
        dates_in_month(tl dates, month)
    end

fun dates_in_months(dates: (int*int*int) list, months: int list) = 
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) = 
  if n > 1
  then get_nth(tl strings, n - 1)
  else hd strings

fun date_to_string(date: int*int*int) = 
  let val months = ["January ", "February ", "March ", "April ", "May ", "June ", "July ",
  "August ", "September ", "October ", "November ", "December "]
  in get_nth(months, #2 date) ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1
  date)
  end

fun number_before_reaching_sum(sum: int, nums: int list) = 
  if null nums
  then 0
  else if hd nums < sum
  then 1 + number_before_reaching_sum(sum - hd nums, tl nums)
  else 0

fun what_month(day: int) = 
  let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(day, month_days) + 1
  end

fun month_range(day1: int, day2: int) = 
  if day1 > day2
  then []
  else
    let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      what_month day1 :: month_range(day1 + 1, day2)
    end

fun oldest(dates: (int*int*int) list) = 
  if null dates
  then NONE
  else
    let 
      fun get_older(dates: (int*int*int) list) = 
        if null (tl dates)
        then hd dates
        else 
          let val older_date = get_older(tl dates)
          in
            if is_older(hd dates, older_date)
            then hd dates
            else older_date
          end
    in
      SOME(get_older(dates))
    end
    
