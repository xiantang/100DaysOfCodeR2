(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(*Your solutions must use pattern-matching. You may not use the functions null, hd, tl, isSome, or valOf,
nor may you use anything containing a # character or features not used in class (such as mutation).*)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
	     
fun append(xs,ys) =
    if null xs
    then ys
    else (hd xs) :: (append (tl xs,ys))
			
(* put your solutions for problem 1 here *)
fun all_except_option(str,str_list) =
    case str_list of
	[] => NONE
      | hd::tl => if same_string(hd,str) then SOME(tl)
		  else
		      case all_except_option(str,tl) of
			  NONE => NONE
			       | SOME(x) => SOME(hd::x)


fun get_substitustions1 (strll,s)=
    case strll of
	[] => []
      | hd::tl =>
	case all_except_option(s,hd) of
	    SOME(x) =>append( x, get_substitustions1(tl,s))
	  | NONE => get_substitustions1(tl,s)
				       
fun get_substitutions2 (strll,s) =
    let
	fun helper(strll,cur) =
	    case strll of
		[] => cur
	      | hd::tl =>
		case all_except_option(s,hd) of
		    SOME(x) =>helper(tl,append(x,cur))
		  | NONE => helper(tl,cur)

    in
	helper(strll,[])
    end
	
fun similar_names (ssl: string list list, {first=f, middle=m, last=z}) =
    let
	val names = get_substitutions2(ssl,f)
	fun helper(namel) =
	    case namel of
		[] => []
	      | hd::tl => {first=hd,last=z,middle=m} :: helper(tl)
    in
	helper(names)
    end
	

				      
				       
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
