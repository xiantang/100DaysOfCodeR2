structure MyMathLib =
struct

fun fact x =
    if x = 0
    then 1
    else x*fact (x-1)
		
fun doubler y = y+y

end
