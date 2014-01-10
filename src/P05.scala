import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P05 (*) Reverse a list.
Example:
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
*/

def reverse[T](l: List[T]): List[T] = {
  @tailrec
  def helper(rest: List[T], acc: List[T]): List[T] = {
    rest match {
      case Nil => acc
      case h :: t => helper(t, h :: acc)
    }
  }

  helper(l, Nil)
}

println(reverse(List(1, 1, 2, 3, 5, 8)))