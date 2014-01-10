import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P04 (*) Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
*/

def length[T](l: List[T]): Int = {
  @tailrec
  def helper(l: List[T], count: Int): Int = {
    l match {
      case Nil => count
      case _ :: t => helper(t, count + 1)
    }
  }

  helper(l, 0)
}

println(length(List(1, 1, 2, 3, 5, 8)))
