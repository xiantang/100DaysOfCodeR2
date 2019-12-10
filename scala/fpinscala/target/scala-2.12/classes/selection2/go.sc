def factorial(n:Int):Int = {
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
// 上下两行相等 apply 方法可以将对象像方法一样调用
(a:Int,b:Int)=>a<b
val lessThan = new Function2[Int, Int, Boolean] {
  override def apply(a: Int, b: Int): Boolean = a < b
}