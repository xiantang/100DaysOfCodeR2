datatype mytype = TwoInts of int * int
		| Str of string
                | Pizza


(* mytype -> int *)
fun f (x:mytype) =
    case x of
	Pizza => 3 
      | Str s=> String.size s 
      | TwoInts(i1,i2)=> i1+i2
				



datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp *exp

				    
fun eval e  = (* exp -> int *)
    case e of
	Constant i => i
      | Negate e2  =>  ~ (eval e2)
      | Add(e1,e2)  => (eval e1) + (eval e2)
      | Multiply(e1,e2)  => (eval e1)*(eval e2)

(* 先声明一些 datatype *)
					  
datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

						       
type card = suit * rank


type name_record = {
    student_num : int option,
    first : string,
    middle : string option,
    last : string}
		       
					  
fun is_Queen_of_Spades(c:card) =
    #1 c = Spade andalso #2 c =Queen
