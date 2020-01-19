package fpscala

import java.util.concurrent.ForkJoinWorkerThread

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
 * @author zhujingdi
 * @since 2020/1/13
 */
object test extends App {

  import scala.concurrent._


  Await.result(Future.sequence((0 to 100) map  { n =>
    Future {
      println("starting Future: " + n)
      blocking {
        val t  = Thread.currentThread()
        println(t)
        println(t.asInstanceOf[ForkJoinWorkerThread].getPool)
        Thread.sleep(3000)
      }
      println("ending Future: " + n)
    }
  }),Duration.Inf)

}
