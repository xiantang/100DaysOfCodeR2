
(* range 3 6 [3,4,5,6] *)
fun range i j = if i> j then [] else i:: range(i+1) j

(* countup 6 [1,2,3,4,5,6] *)
val countup = range 1
		    


fun exists predicate xs =
    case xs of
	[] => false
		  (* 判断当前存在没有 不存在就调用下面的函数*)
      | x::xs' => predicate x orelse exists predicate xs'
					    
val no = exists (fn x => x=7) [4,11,23]

(* in list -> bool *)
val hasZero = exists (fn x=> x = 0)
		   
(*fun pairWithOne  = List.map (fn x => (x,1)) 
*)

fun curry f x y = f (x,y)

fun uncurry f (x,y) = f x y
			
