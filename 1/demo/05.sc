import scala.annotation.tailrec

// Tail recursion

def rec(n: Int): BigInt = if (n == 1) { 1 } else { rec(n - 1) * n }

rec(100)