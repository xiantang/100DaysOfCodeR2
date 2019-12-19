package selection3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case object NilT extends Tree[Nothing]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 练习 3.25 实现size 统计一棵树的节点数
  def size[A](tree: Tree[A]): Int = tree match {
    case NilT => 0
    case Leaf(_) => 1
    case Branch(left, right) =>
      1 + size(left) + size(right)
  }

  // 练习 3.26 实现maximum 找到一颗树的最大值
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case NilT => 0
    case Branch(left, right) =>
      maximum(left).max(maximum(right))
  }

  // 练习 3.27 实现 depth 函数 求树深
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case NilT => 0
    case Branch(left, right) =>
      1 + depth(left).max(depth(right))
  }

  // 练习 3.28 实现 map 函数 对树的每个元素进行修改
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case NilT => NilT
    case Branch(left, right) =>
      Branch(map(left)(f), map(right)(f))
  }

  // 练习 3.29 实现一个 fold 函数
  def fold[A, B](tree: Tree[A], acc: B)(f: (A, B) => B): B = tree match {
    case NilT => acc
    case Leaf(x) => f(x,acc)
    case Branch(left,right) =>
      fold(right,fold(left,acc)(f))(f)
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(10), NilT)), Branch(Leaf(1), Leaf(9)))
    println(size(tree))
    println(maximum(tree))
    println(depth(tree))
    println(fold(tree,0)((x,y)=>x+y))
    println(fold(tree,0)((x,y)=>if(x>y)x else y))
    println(fold(tree, 0)((x, y) => y + 1))


  }


}
