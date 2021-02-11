// todo: "try/catch"

try {
  throw new IllegalStateException("123")
} catch {
  case e: IllegalStateException => "bl"
  case _  => "Ok"
}