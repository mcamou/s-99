import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P20 (*) Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.
Example:

scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
*/
            
def removeAt[T](n: Int, l: List[T]): (List[T], T) = {
  val split = l.splitAt(n)
  (split._1 ::: split._2.tail, split._2.head)
}

def removeAt2[T](n: Int, l: List[T]): (List[T], T) = {
  @tailrec
  def helper(n: Int, l: List[T], acc: List[T]): (List[T], T) = {
    (n, l) match {
      case (_, Nil) => throw new IllegalArgumentException("List too short")
      case (0, h :: t) => (acc.reverse ::: t, h)
      case (_, h :: t) => helper(n - 1, t, h :: acc)
    }
  }

  helper(n, l, Nil)
}

val res = removeAt(1, List('a, 'b, 'c, 'd))
if (res == (List('a, 'c, 'd),'b)) println("OK")
else println(s"ERR: $res")

val res2 = removeAt2(1, List('a, 'b, 'c, 'd))
if (res2 == (List('a, 'c, 'd),'b)) println("OK")
else println(s"ERR: $res2")