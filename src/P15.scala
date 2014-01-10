/*
Reference: http://aperiodic.net/phil/scala/s-99
P15 (**) Duplicate the elements of a list a given number of times.
Example:
scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
*/

def duplicateN[T](n: Int, l: List[T]): List[T] = l.flatMap(x => List.fill(n)(x))

val res = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
if (res == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)) println("OK")
else println(s"ERR: $res")
