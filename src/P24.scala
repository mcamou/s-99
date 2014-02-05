import scala.annotation.tailrec
import scala.util.Random

/*
Reference: http://aperiodic.net/phil/scala/s-99
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
Example:
scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
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

def lotto(n: Int, max: Int): List[Int] = {
  randomSelect(n, (1 to max).toList)
}

def lotto2(n: Int, max: Int): List[Int] = {
  val rnd = new Random

  @tailrec
  def helper(acc: List[Int]): List[Int] = {
    if (acc.length >= n) acc
    else {
      val r = rnd.nextInt(max + 1)
      val l = if (acc.contains(r)) acc
      else r +: acc
      helper(l)
    }
  }

  helper(Nil)
}

val res = lotto(6, 49)
if (res.length == 6 && ! res.exists(e => e < 0 || e > 49) && res.toSet.size == 6) println("OK")
else println(s"ERR: $res")

val res2 = lotto(6, 49)
if (res2.length == 6 && ! res2.exists(e => e < 0 || e > 49) && res2.toSet.size == 6) println("OK")
else println(s"ERR: $res2")