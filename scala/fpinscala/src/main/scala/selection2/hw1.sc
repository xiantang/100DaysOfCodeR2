// 练习2.1 写一个递归函数 获取第n个斐波那契数 使用尾递归

def fib(n: Int): Int = {
  @scala.annotation.tailrec
  def go(cur: Int, acc: (Int, Int)): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case n => if (n != cur) {
        go(cur + 1, (acc._2, acc._1 + acc._2))
      } else {
        acc._1 + acc._2
      }
    }
  }

  go(2, (0, 1))
}

fib(0) == 0
fib(1) == 1
fib(2) == 1
fib(3) == 2
fib(4) == 3

// 练习 2.2 实现isSorted 方法，检查给定 Array[A] 是否按给定的比较函数排序

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

  @scala.annotation.tailrec
  def loop(n: Int, acc: A): Boolean = {
    if (n >= as.length) true
    else if (ordered(as(n), acc)) {
      loop(n + 1, as(n))
    } else {
      false
    }
  }

  if (as.length > 1) loop(1, as(0)) else true
}
isSorted(Array(3), (x: Int, y: Int) => if (x > y) true else false)
isSorted(Array(3, 5), (x: Int, y: Int) => if (x > y) true else false)
isSorted(Array(3, 4, 6, 8), (x: Int, y: Int) => if (x > y) true else false)
!isSorted(Array(5, 4, 3, 2), (x: Int, y: Int) => if (x > y) true else false)


// 练习 2.3 柯里化的例子，两个带参数的转换为只有一个参数的部分应用函数

def curry[A, B, C](f: (A, B) => C): A => B => C = {
  a: A => b: B => f(a, b)
}

// 练习 2.4 实现反柯里化

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

// 练习 2.5 实现一个高阶函数将两个函数组合为一个函数

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a: A => f(g(a))
}
