import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P13 (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
Example:

scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/

def encodeDirect[T](l: List[T]): List[(Int, T)] = {
  @tailrec
  def helper(l: List[T], acc: List[(Int, T)]): List[(Int, T)] = {
    l match {
      case Nil => acc.reverse
      case _ =>
        val (h, t) = l.span(_ == l.head)
        helper(t, (h.length, h.head) :: acc)
    }
  }

  helper(l, Nil)
}


val res = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

if (res == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) println("OK")
else println(s"ERR: $res")
