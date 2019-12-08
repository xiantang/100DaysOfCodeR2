


(* 不好的版本 因为只有一个case *)
fun sum_triple1 (triple: int * int *int) =
    case triple of
	(x,y,z) => x+y+z

fun full_name1 (r: {first:string,middle:string,last:string}) =
    case r of
	{first=x,middle = y,last = z } =>
	 x ^ " " ^ y ^ " " ^ z

fun full_name3 {first = x,middle =y ,last = z} =
    x ^ " " ^ y ^ " " ^z

fun sum_triple3 (x,y,z) =
    x + y +z

	       (* 在 ML 中函数的参数实际上只有一个参数 *)
