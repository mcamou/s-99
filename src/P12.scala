/*
Reference: http://aperiodic.net/phil/scala/s-99
P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:

scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
*/

def decode[T](l: List[(Int, T)]): List[T] = {
  l.foldLeft(List.empty[T]) { (memo, elem) => memo ++ List.fill(elem._1)(elem._2) }
}

println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
