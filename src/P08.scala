import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
*/

def compress[T](l: List[T]): List[T] = {
  l.foldLeft(List[T]()) { (memo, elem) =>
    if (memo.isEmpty || elem != memo.last) memo :+ elem
    else memo
                          }
}

def compressR[T](l: List[T]): List[T] = {
  @tailrec
  def helper(rest: List[T], acc: List[T]): List[T] = {
    rest match {
      case Nil => acc
      case h :: Nil => acc :+ h
      case h :: t => helper(t, if (h == t.head) acc else acc :+ h)
    }
  }

  helper(l, Nil)
}

println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println(compressR(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
