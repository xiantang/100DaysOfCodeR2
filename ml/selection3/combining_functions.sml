(* val compose = fn : ('a -> 'b) * ('c -> 'a) -> ('c -> 'b) *)

fun compose(f,g) =
    fn x => f(g x)

	     
fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))
(* 像数学一样 我们的函数计算从右到左 *)
			      (* 获得绝对值 -> 转换为Real -> 拿到平方根 *)
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i
						     
