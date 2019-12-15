package selection5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // 练习 5.1 写一个将 Stream 转换为 List 的函数
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(head, tail) => head() :: tail().toList
  }

  // 练习 5.2 写一个 take(n) 返回 Stream 中前 n 个元素
  def take(n: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(cur: Int, acc: List[A], res: Stream[A]): List[A] = cur match {
      case _ if cur != n => res match {
        case Cons(head, tail) =>
          loop(cur + 1, acc :+ head(), tail())
        case Empty => acc
      }
      case _ => acc

    }
    loop(0, List(), this)
  }

  // 练习 5.2 写一个 drop(n) 返回 Stream 第 n 个元素之后的所有元素
  def drop(n: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(cur: Int, res: Stream[A]): List[A] = cur match {
      case _ if cur != n => res match {
        case Cons(_, tail) =>
          loop(cur + 1, tail())
        case Empty => List()
      }
      case _ => res.toList
    }
    loop(0, this)

  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
    //    val x= Cons(()=>{
    //      println(11)
    //      1+1
    //    },()=>Empty)
    //    x.headOption
    //    x.headOption
    //  }
    println(Stream(1 + 1, 1, 1))
    println(Stream(1 + 1, 1, 1).toList)
    println(Stream().toList)
    println(Stream(1, 23, 4, 5, 6).take(10))
    println(Stream(1, 23, 4, 5, 6).take(4))
    println(Stream(1, 23, 4, 5, 6).drop(3))


  }
}