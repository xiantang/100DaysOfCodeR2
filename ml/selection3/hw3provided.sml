(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals (strl) = List.filter (fn x=> Char.isUpper(String.sub(x,0))) strl

										
val test1 = only_capitals ["A","B","C"] = ["A","B","C"];

fun longest_string1 (strl) =
   foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" strl

val stringList = ["Medet", "Can", "akus", "Linux", "linux", "Ubuntu", "i am Marry Poppins, You all", "I am Marry Poppins, You all"];

val test2 = longest_string1 stringList = "i am Marry Poppins, You all"

fun longest_string2 (strl) =
   foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" strl
	 
val test3 = longest_string2 stringList = "I am Marry Poppins, You all"



	  
fun longest_string_helper f strl =
    case strl of
	[] => ""
        | _ => foldl (fn (x,y) => if (f(String.size(x),String.size(y))) then x else y) "" strl 
fun longest_string3 (strl) =  longest_string_helper (fn (x,y) => x>y) strl
   
fun longest_string4 (strl) =  longest_string_helper (fn (x,y) => x>=y) strl


fun longest_capitalized strl = ( longest_string3 o only_capitals) strl	

val test7 =longest_capitalized ["sss","as","ss"]  = "";

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | hd::tl =>if isSome(f hd) then valOf(f hd) else first_answer f tl	;


fun rev_string str = ( String.implode o List.rev  o String.explode) str
