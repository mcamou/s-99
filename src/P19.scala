/*
Reference: http://aperiodic.net/phil/scala/s-99
P19 (**) Rotate a list N places to the left.
Examples:
scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
*/

def rotate[T](n: Int, l: List[T]) = {
  if (n > 0) l.takeRight(l.length - n) ::: l.take(n)
  else l.takeRight(-n) ::: l.take(l.length + n)
}

def rotate2[T](n: Int, l: List[T]) = {
  val newN = if (n > 0) n else l.length + n
  val split = l.splitAt(newN)
  split._2 ::: split._1
}

val res = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
if (res == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)) println("OK")
else println(s"ERR: $res")

val res2 = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
if (res2 == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)) println("OK")
else println(s"ERR: $res2")

val res3 = rotate2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
if (res3 == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)) println("OK")
else println(s"ERR: $res3")

val res4 = rotate2(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
if (res4 == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)) println("OK")
else println(s"ERR: $res4")