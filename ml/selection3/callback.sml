val cbs :(int -> unit) list ref = ref []

(* 注册一个函数上去*)
				      
fun onKeyEvent f = cbs := f :: (!cbs)

fun onEvent i =
    let fun loop fs =
	    case fs of
		[] => ()
	      | f::fs' => (f i;loop fs')
    in
	loop(!cbs) end

val timesPressed = ref 0

val _ = onKeyEvent ( fn _ => timesPressed := (!timesPressed+1))

fun printIfPressed i =
    onKeyEvent (fn j => if i=j
			then
			    print ("pressed " ^ Int.toString i ^ "\n")
			else
			    ())
(* 添加callback *)
val _ = printIfPressed 4
		       
val _ = printIfPressed 11

val _ = printIfPressed 23

val _ = printIfPressed 4
		       
		       
