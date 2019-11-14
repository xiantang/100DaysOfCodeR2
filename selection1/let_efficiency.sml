

fun good_max(xs :int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
	(* 使用绑定的方式 *)
	let
	    val tal_ans = good_max(tl xs)
	in
	    if hd xs > tal_ans
	    then hd xs
	    else tal_ans
	end
	    
