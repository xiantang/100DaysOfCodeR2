(*

为什么要使用 lexical scope 

lexical scope : 使用环境当函数定义的时候
dynamic scope : 使用环境当函数调用的时候

*)

(* 1.当你使用了 lexical scope 你更改了函数内部的命名
在调用的时候将不会受影响
但是如果你使用的 dynamic scope 将会受影响 *)
fun f1 y = let val x = y+1 in
	       fn z => x+y+z
	   end

fun f2 y = let val q = y+1 in
	       fn z => q+y+z
	   end



(* 2. 使用 lexical scope 可以对函数进行类型检查 *)
val x = 1
fun f y =
    let val x = y+1
    in fn z =>x+y+z end

val x = "hi"
val g = f 7
val z = g 4

(* 3. 可以方便的存储需要的数据 *)
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
	    
fun greaterThenX x = fn y => y > x (* 返回一个闭包 *)

(* greaterThenX 会被绑定为 fn y=> y > -1*)
				    
fun noNegatives xs = filter(greaterThenX ~1,xs)
