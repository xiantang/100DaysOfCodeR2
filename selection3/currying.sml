(* old *)
fun sorted3_tuple (x,y,z) =
    z >= y andalso y>= x

val t1 =  sorted3_tuple(7,9,11)

(* new *)
		       
val sorted3 = fn x => fn y => fn z => z>=y andalso y>=x
							  
val t2 = ((sorted3 7) 9) 11

(* 当你调用 sorted3 7 的时候 
 代码是fn y => fn z => z>=y andalso y>=x 
 环境绑定是 x = 7
   当你调用 这个闭包和参数 9的时候
 代码是fn z => z >= y andalso y> = 7
 环境绑定是 y = 9
  下面是
  z=> z>= 9 andalso 9>7
 代码是 11 所以是true
*)
 
(* 更加舒服的语法糖写法 *)

val t3 = sorted3 7 9 11
			 
fun sorted3_nicer x y z = z>=y andalso y>=x

 (* 使用柯里化的 fold*)
					      
fun fold f acc xs =
    case xs of
	[] => acc
     | hd::tl => fold f (f(acc,hd)) tl
fun sum xs = fold (fn (x,y) => x+y) 0 xs
