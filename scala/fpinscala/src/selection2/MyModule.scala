package selection2

object MyModule {


  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) {
        acc
      } else
      // 尾调用消除
      // 编译器会优化为类似 while 循环的方式
        go(n - 1, acc * n)
    }

    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  /**
   *
   * 单态的函数
   * def findFirst(ss:Array[String],key:String):Int = {
   *
   * @scala.annotation.tailrec
   * def loop(n: Int): Int = {
   * if (n > ss.length) -1
   * else if (ss(n) == key) n
   * else loop(n + 1)
   * }
   * loop(0)
   * }
   */

  /**
   * 用类型 A 取代掉了String类型的硬编码
   *
   * @param as
   * @param p
   * @tparam A
   * @return
   */
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @scala.annotation.tailrec
    def loop(n: Int): Int = {
      if (n > as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }


  def partial[A,B,C](a:A,f:(A,B)=>C):B=>C = {
    b:B => f(a, b)
  }

  // 通常返回 Unit 的方法暗示它包含副作用
  def main(args: Array[String]): Unit
  = {
    println(formatAbs(-42))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial value", 7, factorial))


  }
}
