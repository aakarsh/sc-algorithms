package algorithms.dp

import scala.annotation.tailrec

object Numerics {
      
  def fibSlow(n:BigInt) :BigInt  = 
    n.toInt match {
      case 0 => 0
      case 1 => 1
      case _ => fibSlow(n-1) + fibSlow(n-2)
    }
  
  
  def fib(n: BigInt) : BigInt = {    
    
    var pp  :BigInt = 0
    var p   :BigInt = 1
    var cnt :BigInt = n
    
    while ( cnt >= 0) {
      val t = p
      p = p+pp
      pp = t
      cnt -= 1
    }    
    
    return pp
  }    
  
  def factorial(n: BigInt): BigInt = {    
    @tailrec
    def factorial_r(acc: BigInt, n: BigInt): BigInt = 
      if(n <= 1) acc
      else       factorial_r(acc*n, n-1)
    
    factorial_r(1, n)    
  }
    
  def main(args:Array[String]) = {
    for(i <- 1 to 100) {
      printf("%5d %20d\n",i,factorial(i))
    }
  }
}