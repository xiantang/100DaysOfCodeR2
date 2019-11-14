fun silly(z:int) =
    let
	val x = if z>0 then z else 34
	val y = x+z+9
    in
	if x >y then x * 2 else y*y
    end


fun sill2 () =
    let
	val x = 1
    in
	(let val x = 2 in x +1 end) + (x)
    end
	
	
