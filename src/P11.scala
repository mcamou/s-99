import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/

// From P09
def pack[T](l: List[T]): List[List[T]] = {
  @tailrec
  def helper(l: List[T], acc: List[List[T]]): List[List[T]] = {
    l match {
      case Nil => acc.reverse
      case h :: t => helper(l.dropWhile(_ == h), l.takeWhile(_ == h) :: acc)
    }
  }

  helper(l, Nil)
}

def encode[T](l: List[T]): List[Any] = {
  pack(l).map(l => if (l.length == 1) l.head else (l.length, l.head))
}

println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
