package selection5

// 把函数的描述和求值分离
sealed trait Stream[+A] {
  //  def headOption: Option[A] = this match {
  //    case Empty => None
  //    case Cons(h, _) => Some(h())
  //  }

  // 练习 5.1 写一个将 Stream 转换为 List 的函数
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(head, tail) =>
      val a = head()
      a :: tail().toList
  }

  // 练习 5.2 写一个 take(n) 返回 Stream 中前 n 个元素
  def take(n: Int): Stream[A] = {
    def loop(cur: Int, res: Stream[A]): Stream[A] = cur match {
      case _ if cur != n => res match {
        case Cons(head, tail) =>
          Cons(head, () => loop(cur + 1, tail()))
        case Empty => Empty
      }
      case _ => Empty

    }

    loop(0, this)
  }

  // 练习 5.2 写一个 drop(n) 返回 Stream 第 n 个元素之后的所有元素
  def drop(n: Int): Stream[A] = {
    @scala.annotation.tailrec
    def loop(cur: Int, res: Stream[A]): Stream[A] = cur match {
      case _ if cur != n => res match {
        case Cons(_, tail) =>
          loop(cur + 1, tail())
        case Empty => Stream()
      }
      case _ => res
    }

    loop(0, this)
  }

  //  练习 5.3  写一个函数 takeWhile 返回一个 stream 满足断言
  //  def takeWhile(p: A => Boolean): Stream[A] = {
  //    def loop(next: Stream[A]): Stream[A] = next match {
  //      case Cons(h, t) =>
  //        val v = loop(t())
  //        if (p(h())) {
  //          Cons(h, () => v)
  //        }
  //        else v
  //      case Empty => Empty
  //    }
  //
  //    loop(this)
  //  }

  // 你会当找到之后就不会继续向下走
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) =>
      f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])(
      (a, b) =>
        if (p(a)) Stream.cons(a, b)
        else b
    )
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A]) {
      (a, _) =>
        Some(a)
    }
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])(
      (x, y) =>
        Stream.cons(f(x), y))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) {
      (h, acc) =>
        if (f(h)) Stream.cons(h, acc)
        else acc
    }
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
    println(Stream(1, 23, 4, 5, 6).take(10).toList)
    println(Stream(1, 23, 4, 5, 6).take(4).toList)
    println(Stream(1, 23, 4, 5, 6).drop(3).toList)
    println(Stream(1, 23, 4, 5, 6).takeWhile(x => x % 2 != 0).toList)
    println(Stream(1, 3, 5, 7, 9).forAll(x => x % 2 != 0))
    println(Stream(1, 3, 6, 7, 9).forAll(x => x % 2 != 0))
    println(Stream(1, 3, 6, 7, 9).headOption.get)
    println(Stream(1, 3, 6, 7, 9).map(_ + 1).filter(_ % 2 == 0).toList)
    println(Stream(1, 3, 6, 7, 9).filter(_ % 2 == 0).toList)


  }
}