package selection6


trait RNG {
  def nextInt: (Int, RNG)

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // 状态转换
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

}


case class SimpleRNG(see: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (see * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, newRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng) = nextInt
    if (n >= 0) {
      (n, rng)
    } else {
      nonNegativeInt(rng)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  //  def double(rng: RNG): (Double, RNG) = {
  //    val t = nonNegativeInt(rng)
  //    (t._1.toDouble, t._2)
  //  }


  def double: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @scala.annotation.tailrec
    def loop(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n == 0) {
        (acc, rng)
      } else {
        val int = rng.nextInt
        loop(n - 1, int._1 :: acc, int._2)
      }
    }

    loop(count, List(), rng)
  }
}


object RNG {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    println(rng.int)
    println(rng.unit(1))
    println(rng.nonNegativeEven)
    //    val (n1, rng2) = rng.nextInt
    //    println(rng.nextInt)
    //    println(rng.nonNegativeInt(rng))
    //    println(rng.ints(5)(rng))
    //    rng.int
  }
}