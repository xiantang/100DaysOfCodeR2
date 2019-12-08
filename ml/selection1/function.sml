
(* 在y大于0的情况下工作 *)
fun pow(x:int ,y :int) =
    if y = 0
    then 1
    else x*pow(x,y-1)
(* val pow = fn : int * int -> int	       *)
(* 没有 class*)	      
fun cube (x:int)=
    pow(x,3)

val sixtyfour = cube(4)
val fortytwo = pow(2,2+2)+pow(4,2)+cube(2) +2		    



						
