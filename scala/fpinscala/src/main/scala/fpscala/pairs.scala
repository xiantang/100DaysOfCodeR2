package fpscala

/**
 * @author zhujingdi
 * @since 2020/1/19
 */
object pairs extends App {
  val n = 7
  (1 until n) map ( i =>
    (1 until i) map (j => (i,j))
  )
}
