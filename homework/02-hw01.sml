(* Homework 01*)
(* Grader Output: 
    Because the auto-grader gave a score above 80, here is the link to a message from a very cute dog: https://drive.google.com/file/d/0B5sUgbs6aDNpRDlPRHhkdTVEUXM/view?pref=2&pli=1

    With extra credit you received 104.0%!  Grade capped at 100% *)

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

