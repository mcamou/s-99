import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
Example:

scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
*/
            
def slice[T](start: Int, end: Int, l: List[T]): List[T] = {
  @tailrec
  def helper(start: Int, end: Int, l: List[T], acc: List[T]): List[T] = {
    (end, l) match {
      case (0, _) => acc.reverse
      case (_, Nil) => acc.reverse
      case (_, h :: t) =>
        val newAcc = if (start <= 0) h :: acc else acc
        helper(start - 1, end - 1, t, newAcc)
    }
  }

  helper(start, end, l, Nil)
}

val res = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
if (res == List('d, 'e, 'f, 'g)) println("OK")
else println(s"ERR: $res")