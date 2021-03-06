fun is_older(dt1 : int * int * int, dt2 : int * int * int)=
 #1 dt1 < #1 dt2 orelse 
     #1 dt1 = #1 dt2
        andalso #2 dt1 < #2 dt2
            orelse #2 dt1 = #2 dt2 andalso #3 dt1 < #3 dt2;
		   
fun number_in_month(dates:(int*int*int) list,month:int)=
    if null dates
    then 0
    else
	let
	    fun is_equal(mnth1:int)=
		if mnth1 = month then 1 else 0
	in
	    is_equal(#2 (hd dates)) + number_in_month(tl dates,month)
	end


fun number_in_months(dates:(int*int*int) list,months:int list)=
    if null months
    then 0
    else
	number_in_months(dates,tl months) + number_in_month(dates,hd months)
							   
fun dates_in_month(dates:(int*int*int) list,month:int)=
    if null dates
    then []
    else
	let
	    val date = dates_in_month(tl dates,month)
	in
	if #2 (hd dates) = month then (hd dates)::date else date
	end
	    
fun dates_in_months(dates:(int*int*int) list,months:int list)=
    if null months
    then []
    else
	dates_in_month(dates,hd months) @ dates_in_months(dates,tl months)

	     
fun get_nth(strList:string list,n:int) =
    if 1 = n
    then hd strList
    else
	get_nth(tl strList,n-1)
	       
val monthList = [
    "January", "February", "March", 
    "April", "May", "June", 
    "July", "August", "September", 
    "October", "November", "December"
];	       

	       
fun date_to_string(date : (int * int * int))=
    get_nth(monthList, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date);


fun number_before_reaching_sum(upperBound : int, lst : int list)=
  if null lst
  then 0
  else 
    if  upperBound - hd lst <= 0
    then 0
    else 1 + number_before_reaching_sum(upperBound - hd lst, tl lst);

val integers = [ 31,28,31,30,31,30,31,31,30,31,30,31]

fun what_month(days:int)=
    number_before_reaching_sum(days,integers)+1


fun month_range(day1:int,day2:int)=
    if day1 > day2
    then []
    else
	what_month(day1) :: month_range(day1+1,day2)



fun oldest(dateList : (int * int * int) list)=
    if null dateList
    then NONE
    else
        let 
            val tmp = oldest(tl dateList)
        in 
            if isSome tmp andalso is_older(valOf tmp, (hd dateList))
            then tmp 
            else SOME (hd dateList)
        end;

