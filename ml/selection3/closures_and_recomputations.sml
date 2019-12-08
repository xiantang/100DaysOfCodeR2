fun filter(f,xs) =
    case xs of
	[] => []
      | x::xs' =>
	let
	    val res = filter(f,xs')
	in
	    if(f x) then x :: res else res
	end

fun allShorterThan1 (xs,s) =
    filter(fn x => String.size x < String.size s,xs)

	  (* 减少 重新计算 *)
fun allShorterThan2(xs,s) =
    let val i = String.size s in
	filter(fn x=> String.size x < i,xs)
    end
	
