
/*
Reference: http://aperiodic.net/phil/scala/s-99
P14 (*) Duplicate the elements of a list.
Example:
scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
*/

def duplicate[T](l: List[T]): List[T] = {
  l.foldLeft(List.empty[T])((memo, el) => el :: el :: memo).reverse
}

val res = duplicate(List('a, 'b, 'c, 'c, 'd))

if (res == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)) println("OK")
else println(s"Err: $res")
