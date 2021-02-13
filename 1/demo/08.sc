import java.lang.IllegalArgumentException

// todo: "try/catch"
lazy val x = {
  println("x");
  123
}
println("foo")
x

try {
  throw new RuntimeException
} catch {
  case e: RuntimeException => print(s"ex ${e.getMessage}")
  case _: IllegalArgumentException =>
}