
fun append(xs,ys) =
    case xs of
	[] => ys
      | x :: xs' => x :: append(xs',ys)

exception ListLengthMismatch
			       
fun zip list_triple =
    case list_triple of
	([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip(tl1,tl2,tl3)
      | _ => raise ListLengthMismatch


fun unzip tuple_triple =
    case tuple_triple of
	[] => ([],[],[])
      | (hd1,hd2,hd3)::tl => let val (l1,l2,l3) = unzip tl 
			     in
				 (hd1::l1,hd2::l2,hd3::l3)
			     end
				 
