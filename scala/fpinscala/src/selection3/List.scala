package selection3

// 泛型的数据类型
// sealed 所有实现必须在这个文件里面
sealed trait List[+A]

case object Nil extends List[Nothing] // 空的List

case class Cons[+A](head: A, tail: List[A]) extends List[A] // 非空的List 尾部可能是一个Cons 或者 Nil

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }


  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else {
      Cons(as.head, apply(as.tail: _*))
    }


  }
}
