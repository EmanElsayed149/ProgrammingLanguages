fun is_older(d1:int*int*int , d2:int*int*int)=
    #1 d1< #1 d2 orelse(( #1 d1) =( #1 d2) andalso ((#2 d1) < (#2 d2) orelse (( #2 d1) =( #2 d2) andalso( #3 d1) <( #3 d2))))

fun number_in_month(ld:(int*int*int)list,month:int)=
    if null ld
    then 0
    else if #2(hd ld)=month
    then 1+number_in_month(tl ld,month)
    else number_in_month(tl ld,month)
fun number_in_months(ld :(int*int*int)list,lm :int list)=
    if null lm 
    then 0
    else
	number_in_month(ld,hd lm)+number_in_months(ld,tl lm)
fun dates_in_month(ld:(int*int*int) list ,m:int)=
    if null ld
    then []
    else if #2(hd ld )=m
    then (hd ld)::dates_in_month(tl ld,m)
    else dates_in_month(tl ld,m)
fun dates_in_months(ld:(int*int*int)list ,lm:int list)=
    if null lm
    then []
    else dates_in_month(ld,hd lm) @ dates_in_months(ld,tl lm)
						   
fun get_nth(los:string list,n:int)=
    if null los
    then ""
    else if  n=1 orelse null (tl los)
    then hd los
    else get_nth(tl los,n-1)
fun month_to_string(month:int)=
	    if month = 1
	    then "January"
	    else if month=2
	    then "February"
	    else if month=3
	    then "March"
	    else if month = 4
	    then "April"
	    else if month =5
	    then "May"
	    else if month = 6
	    then "June"
	    else if month = 7
	    then "July"
	    else if month =8 
            then "August"
            else if month = 9 
            then "September"
            else if month = 10 
            then "October"
            else if month =11
            then "November"
            else "December"
fun date_to_string(d:int*int*int)=
    month_to_string(#2 d) ^" "^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)


fun number_before_reaching_sum(sum:int,lon:int list)=
    if sum <= 0
    then 0-1
    else if  null lon
    then 0
    else 1+number_before_reaching_sum(sum-(hd lon),tl lon)



fun what_month(d :int)=
    let val value=[31,28,31,30,31,30,31,31,30,31,30,31]
    in 1+ number_before_reaching_sum(d,value)
    end
fun month_range(day1:int,day2:int)=
    if day1  > day2
    then []
    else what_month(day1) :: month_range(day1 + 1,day2)
					
fun oldest(lod : (int*int*int) list)=
    if null lod
    then NONE
    else
	let val min=oldest(tl lod)
	in
	    if isSome min andalso is_older( valOf min,hd lod)
	    then min
	    else SOME (hd lod)
	end
	    

