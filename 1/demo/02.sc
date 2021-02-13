//List
import scala.collection.mutable._

val list = List(1, 2, 3)
println(list)

val (x :: xs) = list
println(x)

val a = 1::2::3::Nil

//todo: map
list.map(_ * 2)
//todo: filter
list.filter(_ % 2 != 0)
//todo: foldLeft, fl0a9tMap
