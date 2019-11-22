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
	      (*takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red)*)
fun card_color(card:card)=
    case card of
	(Clubs,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red
      | (Spades,_) => Black
		      
fun card_value(card:card) =
    case card of
	(_,Num value) => value 
      | (_,Ace) => 11
      | _ => 10

(* 写法很多 也都很有意思 *)
fun all_same_color(colors) =
    case colors of
	[] => true
      | _::[] =>  true
      | c1 :: c2 :: t => card_color c1 = card_color c2
			 andalso all_same_color(c2::t)




fun same_card(card1:card,card2:card) =
    card1 = card2
		 
fun all_except_option1(str,str_list) =
    case str_list of
	[] => NONE
      | hd::tl => if same_card(hd,str) then SOME(tl)
		  else
		      case all_except_option1(str,tl) of
			  NONE => NONE
			       | SOME(x) => SOME(hd::x)


fun remove_card(cardlist,c,ex) =
    let
	val except =  all_except_option1(c,cardlist)
    in
	if (except = SOME(cardlist))
	then raise ex
	else
	    case except of
		SOME(A) => A
	   
    end

(* 使用尾递归哈*)	
fun sum_cards(list_card) =
    let
	fun helper(sum,cards) =
	    case cards of
		[] => sum
	      | hd::tl => helper(sum+card_value hd,tl)
    in
	helper(0,list_card)
    end


(* 计算分数*)
fun score(cards,goal)  =
    let
	val sum = sum_cards(cards)
    in
	abs(sum - goal) div 2
    end
	
	(*
						
fun officiate(cards,moves,goal) =
    let
	fun helper(cardss,movess) =
	    case movess of
		[] => []
	      | hd::tl =>
		let
		    val head = hd cards
		in
		    case hd of
			Discard => helper(remove_card(cards,head,IllegalMove),tl)
		      | Draw=>  hd ::  helper(remove_card(cards,x,IllegalMove),tl)
		end
    in
	score(helper(cards,moves),goal)
    end
	
	*)
(*
	fun helper(cardss,movess) =
	    case movess of
		[] => []
	      | hd::tl =>
		let
		    val head = hd cardss
		in
		    case hd of
			Discard => helper(remove_card(cards,head,IllegalMove),tl)
		      | Draw=>  hd ::  helper(remove_card(cardss,hd,IllegalMove),tl)
		end
*)

fun officiate(cards,moves,goal)=
    let
	fun helper(cards,held,moves) =
	    case moves of
		[] => held
	      | cur::tl =>
		case  cur of
		    Discard(x) => helper(cards,remove_card(held,x,IllegalMove),tl)
		 |  Drew =>
		    let
			val hdd = hd cards
		    in
			helper(remove_card(cards,hdd,IllegalMove),hdd ::held,tl)
		    end
			
    in
	score(helper(cards,[],moves),goal)
    end
	


	 
