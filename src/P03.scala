import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
*/

@tailrec
def nth[T](count: Int, l: List[T]): T = {
  l match {
    case h :: _ if count == 0 => h
    case _ :: t => nth(count - 1, t)
    case Nil => throw new NoSuchElementException
  }
}

println(nth(2, List(1, 1, 2, 3, 5, 8)))