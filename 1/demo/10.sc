import java.io.File

//Local functions

def foo() = {
  def bar() = {

  }
  bar()
}

//More about functions:

//todo: repeated parameters

def foo1(a: Int*) = {
  a.sum
}
foo1(1, 2, 3, 4)

//todo: named parameters

//todo: default arguments

def foo1(a: Int = 10) = {
  a
}

//todo: by-name parameters

def foo2(a: => String) = {
  println("foo2")
  println(a)
}

def str = {
  println("str")
  "foo"
}

foo2(str)


//todo: currying

def withResources(f: File)(body: File => Unit): Unit = {

}

withResources(???) { f =>
  f.w
}