import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99

P01 (*) Find the last element of a list.
Example:
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
*/

@tailrec
def last[T](l: List[T]): T = {
  l match {
    case Nil => throw new NoSuchElementException("Empty list is invalid")
    case x :: Nil => x
    case h :: t => last(t)
  }
}

println(last(List(1, 1, 2, 3, 5, 8)))