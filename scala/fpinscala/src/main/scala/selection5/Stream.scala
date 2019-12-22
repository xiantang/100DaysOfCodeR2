package selection5


// 把函数的描述和求值分离
sealed trait Stream[+A] {

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

  def _take(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
      case _ => None
    }
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

  def _takeWhile(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }


  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
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
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def _map[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) {
      (h, acc) =>
        if (f(h)) Stream.cons(h, acc)
        else acc
    }
  }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a) append b)
  }

  // 复用 filter 来实现 find
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption


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



  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))


  def from(a: Int): Stream[Int] =
    Stream.cons(a, from(a + 1))



  def fibs(): Stream[Int] = {
    def help(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a + b, help(b, a + b))
    }

    Stream(0).append(Stream(1)).append(help(0, 1))
  }


  // 5.11 被称为共递归 守护递归
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def _ones(): Stream[Int] =
    unfold(1) { _ =>
      Some(1, 1)
    }

  def _from(a: Int): Stream[Int] =
    unfold(a) {
      x =>
        Some(x, x + 1)
    }

  def _constant[A](a: A): Stream[A] =
    unfold(a) { _ =>
      Some(a, a)
    }


}

object StreamTest extends App {
  //def main(args: Array[String]) {

  //why compile error: forward reference extends over definition of value ones
  //  val ones: Stream[Int] = Stream.cons(1, ones)
  println(Stream(1, 2, 3, 5)._map(_ + 1).toList)
  println(Stream.constant(6).take(5).toList)
  println(Stream.from(6).take(5).toList)
  println(Stream.fibs().take(6).toList)
  println(Stream.unfold(1) {
    x =>
      if (x == 6) {
        None
      }
      else {
        Some(x, x + 1)
      }
  }.toList)
  //}
  println(Stream.from(6).take(5).toList)
  println(Stream._constant(6)._take(6).toList)
  println(Stream(1, 2, 3, 5)._takeWhile(_%2==1).toList)
  println(Stream.cons(Stream.empty,Stream.empty))
}