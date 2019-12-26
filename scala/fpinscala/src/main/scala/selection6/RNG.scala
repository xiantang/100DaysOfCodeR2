package selection6

trait RNG {
  def nextInt:(Int,RNG)

}


case class SimpleRNG(see:Long)extends RNG{
  override def nextInt: (Int, RNG) = {
    val newSeed = (see * 0x5DEECE66DL + 0xBL)&0xFFFFFFFFFFFFL
    val newRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,newRNG)
  }
  def nonNegativeInt(rng:RNG):(Int,RNG)={
    val (n,rng) = nextInt
    if(n>=0){
      (n,rng)
    }else {
      nonNegativeInt(rng)
    }
  }

  def double(rng:RNG):(Double,RNG) ={
    val t = nonNegativeInt(rng)
    (t._1.toDouble,t._2)
  }

}
object RNG{
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val (n1,rng2) = rng.nextInt
    println(rng.nextInt)
    println(rng.nonNegativeInt(rng))
  }
}