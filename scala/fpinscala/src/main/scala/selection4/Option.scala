package selection4

sealed trait Option[+A] {
  // 如果option不为None 应用f
  // 练习 4.1 对一个 option 内部的数据进行变换
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  // Option 不为 None,对其应用f
  // 练习 4.1 对一个 option 内部的数据转换 传入的函数返回值为 Option 可能是必须要返回值为 Option 的情况比较好用
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  // A 表示 B 类型必须为 A 的父
  // 练习 4.1 返回具体类型，如果为None 就将数据返回
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  // 练习 4.1 返回包装类型，如果为None 就将默认包装返回
  def orElse[B >: A](ob: => B): Option[B] = this match {
    case None => Some(ob)
    case Some(x) => Some(x)
  }

  // 不满足 f Some 转换为 None
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }


  // 现在 mean 是一个完全的函数 每个输入类型都有确定的返回值
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // 练习 4.2 用 flatMap 实现方差函数
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap {
      avg =>
        mean(xs.map(x => math.pow(x - avg, 2)))
    }
  }

  // 练习 4.3 实现泛型函数 map2 组合两个Option
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a: A), Some(b: B)) => Some(f(a, b))
      case _ => None
    }

  // 练习 4.4 写一个 sequence 函数 将Option 列表转换为 Option 一个 如果内部有一个 None 就 None
  def sequence[A](li: List[Option[A]]): Option[List[A]] = {
    @scala.annotation.tailrec
    def loop(n: Int, list: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = n match {
      case _ if n == -1 => acc
      case n => list(n) match {
        case Some(x) => loop(n - 1, list, acc.map(x :: _))
        case None => None
      }
    }

    loop(li.size - 1, li, Some(Nil))
  }

  // 练习 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f))


  def map3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  //    a flatMap {
  //      aa => b map {
  //        bb =>
  //          f(aa,bb)
  //      }
  //    }
  // using for
    for {
      bb <- b
      aa <- a
    } yield f(aa, bb)


  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), None, Some(1))))
    println(sequence(List(Some(1), Some(2), Some(1))))
    println(traverse(List(1, 2, 3, 4, 5))(x => if (x % 2 == 0) None else Some(x)))
    Some(1).flatMap(x => Some(x + 1))
    Some(1).flatMap(x => Some(x.toString))
    Some(1).map(x => x + 1)
    Some(1).map(x => x.toString)

  }
}


case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

}