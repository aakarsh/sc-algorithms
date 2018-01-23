package algorithms.dp

object Numerics {
      
  def fibSlow(n:BigInt) :BigInt  = {
    n.toInt match {
      case 0 => 0
      case 1 => 1
      case _ => fibSlow(n-1) + fibSlow(n-2)
    }
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
  
}