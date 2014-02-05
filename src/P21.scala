import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
(*) Insert an element at a given position into a list.
Example:
scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
*/

def insertAt[T](e: T, i: Int, l: List[T]): List[T] = {
  val split = l.splitAt(i)
  split._1 ++ (e :: split._2)
}

def insertAt2[T](e: T, i: Int, l: List[T]): List[T] = {
  @tailrec
  def helper(i: Int, right: List[T], left: List[T]): List[T] = {
    if (i == 0) left.reverse ++ (e :: right)
    else helper (i-1, right.tail, right.head :: left)
  }

  helper(i, l, Nil)
}

val res = insertAt('new, 1, List('a, 'b, 'c, 'd))
if (res == List('a, 'new, 'b, 'c, 'd)) println("OK")
else println(s"ERR: $res")

val res2 = insertAt2('new, 1, List('a, 'b, 'c, 'd))
if (res2 == List('a, 'new, 'b, 'c, 'd)) println("OK")
else println(s"ERR: $res2")
