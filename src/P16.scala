import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P16 (**) Drop every Nth element from a list.
Example:
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
*/

def drop[T](n: Int, l: List[T]): List[T] = {
  @tailrec
  def helper(n: Int, origN: Int, l: List[T], acc: List[T]): List[T] = {
    val newN = if (n == 0) origN else n - 1

    l match {
      case Nil => acc.reverse
      case h :: Nil => (h :: acc).reverse
      case h :: t =>
        val tmp = if (n != 0) h :: acc
        else acc

        helper(newN, origN, t, tmp)
    }
  }

  helper(n-1, n-1, l, Nil)
}

val res = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
if (res == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)) println("OK")
else println(s"ERR: $res")