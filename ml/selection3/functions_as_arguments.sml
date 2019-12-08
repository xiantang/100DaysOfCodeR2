fun increment_n_times_lame(n,x) =
    if n = 0
    then x
    else
	1+ increment_n_times_lame(n-1,x)


fun double_n_times_lame (n,x) =
    if n = 0
    then x
    else 2*double_n_times_lame(n-1,x)


fun nth_tail_lame(n,xs) =
    if n = 0
    then xs
    else tl (nth_tail_lame(n-1,xs))

(* val n_times = fn : ('a -> 'a) * int * 'a -> 'a *)
(* simpler but less useful: (int -> int) * int * int -> int *)
fun n_times(f,n,x) =
    if n = 0
    then x
    else f(n_times(f,n-1,x))

fun increment x = x+1
fun double x = x +x
val x1 = n_times(double,4,7)
val x2 = n_times(increment,4,7)
val x3 = n_times(tl,2,[4,8,12,16])

(*  因为x = 0 所以 x int *)
(* 因为 f 的返回值是 x(int -> int) *)
(* 因为 1+ times() 返回值是int (int->int) * int -> int *)
fun times_until_zero(f,x) =
    if x = 0
    then 0
    else 1+ times_until_zero(f,f(x))



fun triple_n_times(n,x) =
    (* 函数绑定是错的 需要有一个表达式 *)
    (* n_times((fun trip y = 3*y),n,x) *)
    (* 匿名函数是一个表达式 *)
    n_times((fn x => 3*x),n,x)

(* 不要在匿名函数中使用递归，因为连名字都没有*)

(*函数绑定 其实是val 绑定函数的一个语法糖*)

fun triple x = 3*x

(* 不好的风格 *)
val triple = fn y => 3*y
			   
	   
