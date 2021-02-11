//todo: What is Scala?

val x = 10

def isPrime(x: Int): Boolean = x > 1 && (2 to Math.floor(Math.sqrt(x)).toInt).forall(x % _ != 0)

//define method isPrime

for {
  i <- 1 to 100
  if isPrime(i)
} print(s"$i ")


//print all primes from 1 to 100

