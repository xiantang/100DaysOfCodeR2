(* 他的type是这个 int ref *)
val x = ref 42
val y = ref 42
val z = x
	    (* x-> 42 <- z    y->42 *)
val _ = x:= 43

val w = (!y)+(!z);
(* 注意 ⚠️x 引用不会改变 但是可以改变指向的内容 *)
