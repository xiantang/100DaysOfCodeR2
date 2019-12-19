package selection4

sealed trait Either[+E, +A] {
  // 现在 mean 是一个完全的函数 每个输入类型都有确定的返回值
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list?")
    else Right(xs.sum / xs.length)

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(v) => Left(v)
  }


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {

    @scala.annotation.tailrec
    def loop(n: Int, acc: Either[E, List[A]]): Either[E, List[A]] = n match {
      case -1 => acc
      case _ => es(n) match {
        case Right(v) => loop(n - 1, acc.map(z => v :: z))
        case Left(value) => Left(value)
      }
    }

    loop(es.length - 1, Right(List()))

  }


  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequence(a.map(f))

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def main(args: Array[String]): Unit = {
    val unit = Right(1)
    unit.map(_.toString)
    println(sequence(List(Left(1))))
    println(sequence(List(Left(1), Left(2), Right(1))))
    println(sequence(List(Right(1), Right(2), Right(1))))
    println(traverse(List(1, 2, 3, 4))(
      x =>
        if (x % 1 == 0) Right(x) else Left(x)
    ))


  }

}
