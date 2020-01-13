package fpscala

/**
 * @author zhujingdi
 * @since 2020/1/7
 */
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  @scala.annotation.tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer: Int = x / g

  def denom: Int = y / g

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational): Rational = add(that.neg)

  def less(that: Rational): Boolean = numer * that.denom < that.numer * denom

  def max(that: Rational): Rational = if (that.less(that)) that else this

  override def toString: String = numer + "/" + denom
}

object Rational extends App {
  val rational = new Rational(1, 2)
  println(rational.denom)
}