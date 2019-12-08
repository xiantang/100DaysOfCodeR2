
(*
 f :T1 -> T2
 x :T1 
 T1 = T3*T4  val (y,z) = x
  
 T1 = int [abs has type int -> int]
 T4 = int 
 T1 = int * int 
 so  (abs y) + z :int 
*)

fun f x =
    let val (y,z) = x in
	(abs y)+z
    end
	

fun length xs =
    case xs of
	[] => 0
      | x::xs' => 1+ (length xs');

(*
val f = fn : 'a * 'a * 'b -> 'a * 'a * 'b
f : T1*T2*T3 -> T4

T4 = T1 * T2 * T3
T4 = T2 * T1 * T3

然后放在一起 T1 * T1 * T3 -> T1 * T1 * T3
*)
fun f(x,y,z) =
    if true
    then (x,y,z)
    else (y,x,z)
	     
