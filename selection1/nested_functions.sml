
		     
fun countup_from1(x:int) =
    let
	(* 输出 from 到 to 的所有 *)
	fun count(from :int)=
	    if from = x
	    then x :: []
	    else
		from :: count(from +1)
    in
	count(1)
    end
	
