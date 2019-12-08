val x = 1

fun f y =
    let
	val x = y+1
    in
	fn z => x+y+z (* 返回 2y+1+z*)
    end
	
val x = 3

val g = f 4 (* 返回一个函数 body为 9+z *)

val y = 5
val z = g 6 (* 返回 15*)
	  
