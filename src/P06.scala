import scala.annotation.tailrec

/*
Reference: http://aperiodic.net/phil/scala/s-99
P06 (*) Find out whether a list is a palindrome.
Example:
scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true
*/

// From P05
def reverse[T](l: List[T]): List[T] = {
  @tailrec
  def helper(rest: List[T], acc: List[T]): List[T] = {
    rest match {
      case Nil => acc
      case h :: t => helper(t, h :: acc)
    }
  }

  helper(l, Nil)
}

def isPalindrome(l: List[_]): Boolean = {
  l.reverse == l
}

println(isPalindrome(List(1, 2, 3, 2, 1)))
println(isPalindrome(List(1, 3, 3, 2, 1)))
