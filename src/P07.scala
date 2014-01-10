/*
Reference: http://aperiodic.net/phil/scala/s-99
P07 (**) Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
*/

def flatten(l: List[_]): List[Any] = {
  l match {
    case Nil => Nil
    case h :: t => h match {
      case ls: List[_] => flatten(ls) ::: flatten(t)
      case _ => h :: flatten(t)
    }
  }
}

println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
println(flatten(List(1, 1, 2, 3, 5, 8)))
            
