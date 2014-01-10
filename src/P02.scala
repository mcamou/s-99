import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99

P02 (*) Find the last but one element of a list.
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
*/
            
@tailrec
def penultimate[T](l: List[T]): T = {
  l match {
    case Nil | _ :: Nil => throw new NoSuchElementException
    case x :: y :: Nil => x
    case h :: t => penultimate(t)
  }
}

println(penultimate(List(1, 1, 2, 3, 5, 8)))