(* 对于每个element 实行 f 函数的操作 *)
(* ('a->'b) * 'a list-> 'b list *)

fun map(f,xs) =
    case xs of
	[] => []
      | x::xs' =>  (f x) ::  map(f,xs')

val x1 = map((fn x => x +1,[1,23,43,45234,33]))

(* 使用了 currying *)
fun filter(f,xs) =
    case xs of
	[] => []
      | x::xs' =>
	let
	    val res = filter(f,xs')
	in
	    if(f x) then x :: res else res
	end
	    

fun fold (f,acc,xs) =
    case xs of
	[] => acc
     | x::xs  => fold(f,f(acc,x),xs) 


	    
