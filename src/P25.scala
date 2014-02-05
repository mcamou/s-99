import scala.annotation.tailrec
import scala.util.Random

/*
Reference: http://aperiodic.net/phil/scala/s-99
P25 (*) Generate a random permutation of the elements of a list.
Hint: Use the solution of problem P23.
Example:

scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
*/

// From P23.scala
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

def randomPermute[T](l: List[T]): List[T] = randomSelect(l.size, l)

val list = List('a, 'b, 'c, 'd, 'e, 'f)
val res = randomPermute(list)
if (res.intersect(list).size == list.size) println(s"OK: $res")
else println(s"ERR: $res")