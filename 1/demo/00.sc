//todo: What is Scala?

val some = 100

//define method isPrime
def isPrime(n: Int): Boolean = n > 1 && ((2 until n)).forall((n % _ != 0))

isPrime(7)
isPrime(10)
//print all primes from 1 to 100

for {
  i <- 1 to 100
  if isPrime(i)
} { print(s"$i ")}
