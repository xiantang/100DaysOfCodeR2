import scala.concurrent.Future

def iteratorLast[T](iter: Iterator[T]): Option[T] = {

  @scala.annotation.tailrec
  def loop(acc:Option[T]):Option[T] = if (iter.hasNext) {
    loop(acc = Some(iter.next))
  } else {
    acc
  }

  loop(acc = None)

}


