import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P17 (*) Split a list into two parts.
The length of the first part is given. Use a Tuple for your result.
Example:

scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
*/

def split[T](n: Int, l: List[T]): (List[T], List[T]) = {
  @tailrec
  def helper(n: Int, l: List[T], acc: List[T]): (List[T], List[T]) = {
    if (n < 0) throw new IllegalArgumentException("Negative index")
    if (n > l.length) throw new IllegalArgumentException("Index too large")

    (n, l) match {
      case (0, _) => (acc.reverse, l)
      case (_, Nil) => throw new IllegalArgumentException("List too short")
      case (_, h :: t) => helper(n-1, t, h :: acc)
    }
  }

  helper(n, l, Nil)
}

val res = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
if (res == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) println("OK")
else println(s"ERR: $res")

val res2 = split(0, List(1, 2, 3))
if (res2 == (Nil, List(1,2,3))) println("OK")
else println(s"ERR: $res2")
