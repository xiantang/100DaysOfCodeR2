
datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp


fun max_constant e =
    case e of
	Constant i => i
      | Negate i =>  max_constant(i)
      | Add (i,j) => let val m1 = max_constant i
			 val m2 = max_constant j
		     in
			 if m1 > m2 then m1 else m2
		     end
			 
