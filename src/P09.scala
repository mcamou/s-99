import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
*/

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

println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

