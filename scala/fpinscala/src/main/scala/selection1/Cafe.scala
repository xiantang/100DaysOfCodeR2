package selection1

/**
 * 什么是副作用
 * 修改一个变量的值
 * 直接修改数据结构
 * 设置一个对象的成员
 * 抛出异常
 * ....
 */
class Cafe {

  /**
   *  v1
  def buyCoffee(cc:CreditCard):Coffee = {
    val cup = new Coffee()
    // 这行是有副作用的程序
    // 因为与外部世界产生了交互
    cc.charge(cup.price)
    cup
  }
   */


  def buyCoffee(cc:CreditCard):(Coffee,Charge) = {
    // 函数式的解决方案
    val cup = Coffee()
    // 把费用的创建过程和执行过程分离
    (cup,Charge(cc,cup.price))
  }

  def buyCoffees(cc:CreditCard,n:Int):(List[Coffee],Charge)={
    // List.fill 创建对 x 的多个复制
    val purchases:List[(Coffee,Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees,charges) = purchases.unzip
    (coffees,charges.reduce((x,y)=> x.combine(y)))
  }

  def coalesce(charges:List[Charge]):List[Charge] ={
    charges.groupBy(_.cc).values.map(_.reduce((x,y) => x.combine(y))).toList
  }

}


