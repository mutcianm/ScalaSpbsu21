"Anonymous functions"

//Functional type/

val inc: Int => Int = i => i + 1

//With explicit parameter

val inc2 = (i: Int) => i * i

//Placeholder syntax

val inc3: Int => Int = (_: Int) + 1

inc(10)
inc2(10)
inc3(10)


//partially applied function syntax

def f(a: Int, b: Int, c: Int) = a * b * c

val t = Int => Int = (i => f(10, i, i))

t(3)