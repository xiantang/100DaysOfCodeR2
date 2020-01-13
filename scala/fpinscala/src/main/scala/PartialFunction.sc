/**
 * @author zhujingdi
 * @since 2020/1/10
 */

val sample = 1 to 10
val isEven: PartialFunction[Int, String] = {
  case x if x % 2 == 0 => x+" is even"
}
// the method collect can use isDefinedAt to select which members to collect
//val evenNumbers = sample collect isEven
val isOdd1: PartialFunction[Int, String] = {
  case x if x % 2 == 1 => x+" is odd"
}
//// the method orElse allows chaining another partial function to handle
//// input outside the declared domain
//val numbers = sample map (isEven orElse isOdd)
val f: PartialFunction[Int, Any] = { case _ => 1/0 }
//val f: Function[Int, Any] = { case _ => 1/0 }

//trait Function1[-T1, +R]
//trait PartialFunction[-A, +B]
val isOdd: Function1[Int, String] = {
  case x if x % 2 == 1 => x+" is odd"
}

isOdd1.apply(1)

isOdd1(0).orElse(isEven(0))

