import scala.annotation.tailrec
import scala.util.Random

/*
Reference: http://aperiodic.net/phil/scala/s-99
P23 (**) Extract a given number of randomly selected elements from a list.
Example:
scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
*/

def randomSelect[T](n: Int, l: List[T]): List[T] = {
  val rnd = new Random

  @tailrec
  def helper(n: Int, l: List[T], acc: List[T]): List[T] = {
    if (n == 0) acc
    else {
      val (before, after) = l.splitAt(rnd.nextInt(l.size))
      helper(n - 1, before ++ after.tail, after.head :: acc)
    }
  }
  helper(n, l, Nil)
}

val list = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
val res = randomSelect(3, list)
if (res.length == 3 && res.intersect(list).length == 3) println("OK")
else println(s"ERR: $res")
