import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99

P02 (*) Find the last-Nth element of a list.
Example:
scala> lastNth(4, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
*/

@tailrec
def lastNth[T](n: Int, l: List[T]): T = {
  l match {
    case h :: _ if n == l.size => h
    case _ :: t => lastNth(n, t)
    case _ => throw new NoSuchElementException
  }
}

def lastNth2[T](n: Int, l: List[T]): T = {
  @tailrec
  def helper(count: Int, current: List[T], result: List[T]): T = {
    println(count, current, result)
    current match {
      case Nil if count > 0 => throw new NoSuchElementException
      case Nil => result.head
      case _ :: t => helper(
                             count - 1,
                             t,
                             if (count > 0) result else result.tail
                           )
    }
  }

  helper(n, l, l)
}

println(lastNth(4, List(1, 1, 2, 3, 5, 8)))
println(lastNth2(4, List(1, 1, 2, 3, 5, 8)))