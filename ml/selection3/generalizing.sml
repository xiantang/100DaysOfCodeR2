(* 在我们的例子中一等公民函数有下面几种特性
* 可以传入一个函数作为参数 到另外一个函数
* 返回一个数或者列表 
* 但是一等公民函数还有其他更有用的方式
* 可以 pass 好多函数作为参数
* 可以将函数放到数据结构中
* 返回一个函数作为返回值
* 可以编写一个高阶函数来遍历你的数据结构
*)
(* 愚蠢的例子 真的愚蠢 我都不知道老师为什么用这种傻逼的例子*)
(* 返回一个函数*)
fun double_or_triple f =
    if f 7
    then fn x=> 2*x
    else fn x=>3*x
(* 这是一个函数*)
val double = double_or_triple(fn x=>x -3 =4)

(* 高阶函数不只是面向于 数字或者列表
   他们对于遍历常见的自定义类型的数据结构也适用
*)
						      

datatype exp =
	 Constant of int
	 | Negate of exp
	 | Add of exp*exp
	 | Multiply of exp*exp
			       

fun true_of_all_constants(f,e) =
    case e of
	Constant i => f i
      | Negate e1 => true_of_all_constants(f,e1)
      | Add(e1,e2) => true_of_all_constants(f,e1) andalso true_of_all_constants(f,e2)
      | Multiply(e1,e2) => true_of_all_constants(f,e1) andalso true_of_all_constants(f,e2)

fun all_even e = true_of_all_constants((fn x => (x mod 2) = 0),e)
