package selection3

// 泛型的数据类型
// sealed 所有实现必须在这个文件里面
sealed trait List[+A]

case object Nil extends List[Nothing] // 空的List

case class Cons[+A](head: A, tail: List[A]) extends List[A] // 非空的List 尾部可能是一个Cons 或者 Nil

object List {

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
  }


  // 练习 3.10 尾递归实现 foldLeft
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) =>
      foldLeft(xs, f(x, acc))(f)
  }

  // 练习 3.13 是否可以根据 foldLeft 实现 foldRight
  def foldRight2[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), acc)(f)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)((x, y) => x + y)

  // 练习 3.11
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)((x, y) => x + y)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)((x, y) => x * y)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((_, y) => y + 1)


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)((x, y) => x * y)


  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else {
      Cons(as.head, apply(as.tail: _*))
    }
  }


  // 练习3.4 实现 tail 函数删除list的第一个元素
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // 练习3.5 实现 setHead 函数
  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(_, xs) => Cons(head, xs)
  }

  // 练习3.6 实现 tail 函数
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) =>
      if (n == 0)
        tail
      else Cons(head, drop(tail, n - 1))
  }

  // 练习3.7 实现 dropWhile
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) =>
      if (f(head))
        dropWhile(tail, f)
      else
        Cons(head, dropWhile(tail, f))
  }


  // 这个dropWhile是假的啊 假的假的！
  // 但是利用柯里化的优点可以进行参数推导
  // OS：scala的编译器的类型推导做的不大行，绝大多数的函数式编程语言都不用标注类型了
  @scala.annotation.tailrec
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) =>
      dropWhile2(t)(f)
    case _ =>
      as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(head, tail) => Cons(head, append(tail, a2))
  }

  // 练习 3.6 实现一个init
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  // 练习 3.9 实现 length 函数
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  // 练习 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])(Cons(_, _))


  // 练习 3.14 用foldRight 实现append函数
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, xs) => Cons(x, xs))
  }

  // 练习 3.15 合并多个列表为一个列表
  def flatten[A](a1: List[List[A]]): List[A] = {
    foldRight(a1, Nil: List[A])((x: List[A], xs: List[A]) => append(x, xs))
  }

  // 练习 3.16 写一个函数用来转换整数列表，对每个元素+1
  def addOne(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head + 1, addOne(tail))
  }


  // 练习 3.17 写一个函数将List[String] 中的每一个值转换为 String
  def toStringList(xs: List[Double]): List[String] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head.toString, toStringList(tail))
  }

  // 练习 3.18 实现一个泛化的 map 函数对列表的每个元素进行修改，维持列表结构
  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
  }

  // 练习 3.19 实现一个 filter 函数
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) =>
      val value = filter(tail)(f)
      if (f(head)) Cons(head, value)
      else value
  }

  // 练习 3.20 实现一个 flatMap 函数
  def flatMap[A,B](as:List[A])(f:A=>List[B]):List[B] ={
    def helper(as:List[A])(f:A=>List[B]):List[List[B]] = as match{
      case Nil =>Nil
      case Cons(head, tail) => Cons(f(head), helper(tail)(f))
    }
    flatten(helper(as)(f))
  }

  // 练习 3.21 使用 flatMap 实现 filter
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    List.flatMap(as)(
      x =>
        if (f(x)) Cons(x, Nil)
        else Nil
    )

  // 练习 3.22 接受两个列表对应元素相加
  def zipWithInt(a1:List[Int],a2:List[Int]):List[Int] = (a1,a2) match {
    case (Nil,Nil) => Nil
    case (Cons(head1,tail1),Cons(head2,tail2)) =>
      Cons(head1+head2, zipWithInt(tail1, tail2))
  }

  // 练习 3.23 对 3.22 函数泛化
  def zipWith[A,B](a1:List[A],a2:List[A])(f:(A,A)=>B):List[B] = (a1,a2) match {
    case (Nil,Nil) => Nil
    case (Cons(head1,tail1),Cons(head2,tail2)) =>
      Cons(f(head1,head2), zipWith(tail1, tail2)(f))
  }



  def main(args: Array[String]): Unit = {
    val value = List(1, 3, 4, 5, 4, 3)
    println(List.drop(value, 10))
    val value2 = List(3, 3, 3, 3, 3, 3)
    println(dropWhile(value2, (x: Int) => x == 3))
    println(init(value))
    val value3 = List(1, 2, 3, 4, 5)
    println(dropWhile(value3, (x: Int) => x < 4))
    println(dropWhile2(value3)(x => x < 4))
    // 练习3.7 不行 因为会先不断压入栈中
    val value4 = List(1.2, 2.1, 3.3)
    println(product2(value4))
    println(foldRight(Nil, 0)((x: Int, y) => x + y))
    // 练习3.8 可以用 fold 来遍历或者建立一个List??
    println(List(1, 3, 4, 5) == foldRight2(List(1, 3, 4, 5), Nil: List[Int])(Cons(_, _)))
    println(length(value4))
    println(sum3(value3))
    println(product3(value4))
    println(length2(value4))
    println(reverse(List(1, 23, 4, 6)))
    println(append2(List(1, 2, 3), List(4, 5, 6)))
    println(List(List(2, 3, 5), List(3, 5, 5), List(8, 8, 6)))
    println(filter(List(2,3,5,6,7,8,9))((x:Int)=>x%2==0))
    println(flatMap(List(1,2,3))(i=>List(i,i)))
    println(filter2(List(2,3,5,6,7,8,9))((x:Int)=>x%2==0))
    println(zipWithInt(List(1,2,3),List(4,5,6)))
  }
}
