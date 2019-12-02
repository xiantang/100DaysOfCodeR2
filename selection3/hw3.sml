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

fun check_pat p =
    let
	fun helper_varibles p =
	    case p  of
	        Variable x=>[x]
	      | TupleP ps => List.foldl (fn (p,i) => helper_varibles p @ i) [] ps
	      | _ => []

	fun exists xs =
	    case xs of
		[] => true
	      | x::[] => true
	      | x::xs' =>if ( List.exists (fn a => if (a = x) then false else true) xs') then exists xs' else false
    in
	(exists o helper_varibles) p
    end
	

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | hd::tl =>if isSome(f hd) then valOf(f hd) else first_answer f tl	



fun all_answers f xs =
    let
	fun helper(f,acc,xs) =
	    case xs of
		[] => SOME(acc)
	      | hd::tl=> 
		if isSome(f hd) then helper(f, acc @ valOf(f hd),tl)
		else
		    NONE
    in
	helper(f,[],xs)
    end


	
fun match(var,pat) =
    case (pat, var) of
	(Wildcard, _) => SOME []
      | (Variable s,_) => SOME [(s,var)]
      | (UnitP, Unit) => SOME []
      | (ConstP x, Const y) => if x = y then SOME [] else NONE
      | (TupleP x,Tuple y) => if (List.length x = List.length y) then all_answers match (ListPair.zip(y,x)) else NONE
      | (ConstructorP(s1,p),Constructor(s2,v)) => if s1 = s2 then match(v,p) else NONE 
      | (_,_) => NONE


fun match_first = 
		     
				     

									      

	

(**** for the challenge problem only ****)
fun count_wildcard p =
    g(fn ()=> 1) (fn _ => 0) p


fun count_wild_and_variable_lengths p = g(fn ()=> 1) (fn x => String.size x) p

					
val test9 = count_wild_and_variable_lengths (TupleP [ConstP 10,TupleP [Variable "str"],ConstP 5]) = 3
fun count_some_var(s,p) =
    g(fn ()=>0)(fn x => if x = s then 1 else 0) p
     
     
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

		    



		

    
