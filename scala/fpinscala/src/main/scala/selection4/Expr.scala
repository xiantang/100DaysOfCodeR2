package selection4

/**
 * @author zhujingdi
 * @since 2020/1/13
 */
trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Var(name:String) extends Expr

case class Prod(e1:Expr,e2:Expr) extends Expr


object Number {
  def apply(n: Int): Number = new Number(n)
}

object Sum {
  def apply(e1: Expr, e2: Expr): Sum = new Sum(e1, e2)
}

object exprs{
  def show(e:Expr) :String = e match {
    case Number(x) => x.toString
    case Sum(l,r) => show(l) + " + " + show(r)
    case Var(x) =>
      x.toString
    case Prod(l,r) =>
      val left = l match {
        case Sum(l,r) =>
          "(" + show(l) + " + " +show(r) +")"
        case n => show(n)
      }
      val right = r match {
        case Sum(l,r) =>
          "(" + show(l) + " + " +show(r) +")"
        case n => show(n)
      }
      left + " * " +right
  }

  def main(args: Array[String]): Unit = {
    println(show(Sum(Number(1),Sum(Number(2),Number(1)))))
    println(show(Sum(Prod(Number(2),Var("x")),Var("y"))))
    println(show(Prod(Sum(Number(2),Var("x")),Var("y"))))
  }
}