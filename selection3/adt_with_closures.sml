datatype set = S of {
    insert : int -> set,
    member : int -> bool,
    size : unit -> int} 
			(*client*)

val empty_set =
    let
	fun make_set xs = (* x 作为私有变量 *)
	    let (* 包括私有方法 *)
		fun contains i = List.exists (fn j => i = j) xs
	    in
		S {
		    insert = fn i => if contains i
				     then
					 make_set xs
				     else
					 make_set (i::xs),
		    member = contains,
		    size = fn () => length xs
		}
	    end
    in
	make_set[]
    end
	
	    

val S s1 = empty_set
	       
