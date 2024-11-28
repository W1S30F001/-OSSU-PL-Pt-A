(* Homework 01*)

fun is_older (d1 : (int*int*int), d2 : (int*int*int)) =
    if (#1 d1) = (#1 d2)
    then if (#2 d1) = (#2 d2)
         then if (#3 d1) = (#3 d2)
              then false
              else (#3 d2) > (#3 d1)
         else (#2 d2) > (#2 d1)
    else (#1 d2) > (#1 d1)

fun number_in_month (lod : (int*int*int) list, month : int) = 
    if null lod
    then 0
    else 
        if (#2 (hd lod)) = month
        then 1+number_in_month((tl lod), month)
        else number_in_month((tl lod), month)

fun number_in_months(lod : (int*int*int) list, lom : int list) = 
    if null lom
    then 0
    else number_in_month(lod, (hd lom))+number_in_months(lod, (tl lom))


fun dates_in_month (lod : (int*int*int) list, month : int) = 
    if null lod
    then []
    else 
        if (#2 (hd lod)) = month
        then (hd lod)::dates_in_month((tl lod), month)
        else dates_in_month((tl lod), month)

fun dates_in_months(lod : (int*int*int) list, lom : int list) = 
    if null lom
    then []
    else dates_in_month(lod, (hd lom))@dates_in_months(lod, (tl lom))

fun get_nth(los : string list, n : int) = 
    if n = 1
    then hd los
    else get_nth((tl los), n - 1)

fun date_to_string(date: int*int*int)=
    get_nth(["January","February","March","April","May","June","July","August","September","October","November","December"], (#2 date)) 
    ^ " "
    ^ Int.toString(#3 date) 
    ^ ", " 
    ^ Int.toString(#1 date)

fun number_before_reaching_sum(sum: int, lonumbers: int list) = 
    if sum - (hd lonumbers) <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - (hd lonumbers), (tl lonumbers))

fun what_month(day: int)=
    1 + number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31])

fun month_range(day1: int, day2: int) = 
        if day1 > day2
        then [] 
        else    if day1 = day2
                then [what_month(day2)]
                else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(lodates: (int*int*int) list) =
    if null lodates
    then NONE
    else let fun oldest_nonempty(lodates: (int*int*int) list) = 
                if null (tl lodates)
                then hd lodates
                else    let val tl_oldest = oldest_nonempty(tl lodates)
                        in  if is_older(tl_oldest, (hd lodates))
                            then tl_oldest
                            else hd lodates 
                        end
         in SOME (oldest_nonempty(lodates))
         end

(* Challenger Problems *)

fun number_in_months_challenge(lodates: (int*int*int) list, lomonths: int list) = 
    let fun duplicateQ(suspect, lomonths) =
            if null lomonths
            then false
            else if suspect = (hd lomonths)
                 then true
                 else duplicateQ(suspect, (tl lomonths))

        fun remove_duplicates(lomonths) = 
            if null lomonths
            then []
            else if duplicateQ((hd lomonths), (tl lomonths))
                 then remove_duplicates(tl lomonths)
                 else (hd lomonths)::remove_duplicates(tl lomonths)
    in  let val noduplicate_months = remove_duplicates(lomonths)
        in number_in_months(lodates, noduplicate_months)
        end
    end

fun dates_in_months_challenge(lodates: (int*int*int)list, lomonths: int list) = 
    let fun duplicateQ(suspect, lomonths) =
            if null lomonths
            then false
            else if suspect = (hd lomonths)
                 then true
                 else duplicateQ(suspect, (tl lomonths))

        fun remove_duplicates(lomonths) = 
            if null lomonths
            then []
            else if duplicateQ((hd lomonths), (tl lomonths))
                 then remove_duplicates(tl lomonths)
                 else (hd lomonths)::remove_duplicates(tl lomonths)
    in  let val noduplicate_months = remove_duplicates(lomonths)
        in dates_in_months(lodates, noduplicate_months)
        end
    end

val month_days = [31,28,31,30,31,30,31,31,30,31,30,31]
val month_days_leap_yrs = [31,29,31,30,31,30,31,31,30,31,30,31]

fun reasonable_date(date: int*int*int) = 
    let fun get_month_days(month: int, lomonth_days: int list) =
            if month = 1
            then (hd lomonth_days)
            else get_month_days(month - 1, (tl lomonth_days))

        fun leap_year(year: int) =
            if year mod 400 = 0
            then true
            else year mod 4 = 0 andalso not(year mod 100 = 0) 
    in
    if (#1 date) > 0 andalso (#2 date) >= 1 andalso (#2 date) <= 12 andalso (#3 date) >= 1
    then (#3 date) <= get_month_days((#2 date), if leap_year(#1 date) then month_days_leap_yrs else month_days)
    else false
    end


    (* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test13 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test14 = reasonable_date((2000,1,1)) = true

val test15 = dates_in_months_challenge([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], []) = []

val test16 = dates_in_months_challenge([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [5,2,2,5,5]) = [(1,2,25),(1,2,25),(1,2,27),(3,2,28),(3,5,26)]

val test17 = dates_in_months_challenge([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [2,7,5,7,7,7]) = [(1,2,25),(1,2,25),(1,2,27),(3,2,28),(3,5,26),(6,7,8)]

val test18 = number_in_months_challenge([(1,2,25),(3,2,28),(1,2,27),(1,2,25)], [2,2,12,2,2,12,12,12]) = 4

val test19 = number_in_months_challenge([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], []) = 0