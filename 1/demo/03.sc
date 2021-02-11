import scala.collection.mutable

//Tuple, Map, Set

val t = (1, 2, "123", "456")
val (a, _, _, _) = t

val m = Map(
  1 -> 2,
  5 -> 4,
  15 -> 6
)

m.+((10, 20))
