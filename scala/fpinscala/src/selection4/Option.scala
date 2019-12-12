package selection4

sealed trait Option[+A] {
  // 如果option不为None 应用f
  // 练习 4.1 对一个 option 内部的数据进行变换
  def map[B](f: A => B): Option[B] = {
    case Some(x) => Some(f(x))
    case None => None
  }

  // Option 不为 None,对其应用f
  // 练习 4.1 对一个 option 内部的数据转换 传入的函数返回值为 Option 可能是必须要返回值为 Option 的情况比较好用
  def flatMap[B](f: A => Option[B]): Option[B] = {
    case Some(x) => f(x)
    case None => None
  }

  // A 表示 B 类型必须为 A 的父
  // 练习 4.1 返回具体类型，如果为None 就将数据返回
  def getOrElse[B >: A](default: => B): B = {
    case None => default
    case Some(x) => x
  }

  // 练习 4.1 返回包装类型，如果为None 就将默认包装返回
  def orElse[B >: A](ob: => B): Option[B] = {
    case None => Some(ob)
    case _ => _
  }

  // 不满足 f Some 转换为 None
  def filter(f: A => Boolean): Option[A] = {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }


  // 现在 mean 是一个完全的函数 每个输入类型都有确定的返回值
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // 练习 4.2 用 flatMap 实现方差函数
  def variance(xs:Seq[Double]):Option[Double] ={
    mean(xs) flatMap {
      avg =>
        mean(xs.map( x => math.pow(x - avg, 2)))
    }
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {




  def main(args: Array[String]): Unit = {
    val intToInt = Map[Int, Int](1, 1)
  }

}