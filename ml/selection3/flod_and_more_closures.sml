
(* 从左边开始卷 *)
fun fold(f,acc,xs)=
    case xs of
	[] => acc
      | hd::tl => fold(f,f(acc,hd),tl)


fun sum xs = fold((fn (x,y) => x+y),0,xs); 
fun all_list_non_negative xs = fold((fn (x,y) => x andalso y >=0),true,xs)
				   
fun f4 (xs,s) =
    let val i = String.size s
    in
	fold((fn (x,y) => x andalso String.size y >i),true,xs)
    end
	
