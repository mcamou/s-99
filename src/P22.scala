import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P22 (*) Create a list containing all integers within a given range.
Example:
scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)
*/

def range(start: Int, end: Int): List[Int] = {
  (start to end).toList
}

def range2(start: Int, end: Int): List[Int] = {
  @tailrec
  def helper (end: Int, acc: List[Int]): List[Int] = {
    if (start > end) acc
    else helper(end - 1, end +: acc)
  }

  helper(end, Nil)
}

val res = range(4, 9)
if (res == List(4, 5, 6, 7, 8, 9)) println("OK")
else println(s"ERR: $res")

val res2 = range2(4, 9)
if (res2 == List(4, 5, 6, 7, 8, 9)) println("OK")
else println(s"ERR: $res2")
