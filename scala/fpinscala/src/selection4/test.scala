package selection4

object test extends App {

  // 当你使用 option 时候可以用 lift 对函数进行上升
  def lift[A,B](f:A=>B):Option[A] => Option[B] = _ map f

  val abs0 : Option[Double] => Option[Double] = lift(math.abs)

//  println(Some("1").filter(_=="1"))
}
