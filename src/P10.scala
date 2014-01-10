import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
Example:

scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
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

def encode[T](l: List[T]): List[(Int, T)] = pack(l).map(l => (l.length, l.head))

println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))