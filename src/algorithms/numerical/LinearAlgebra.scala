package algorithms.numerical

object LinearAlgebra {
  
  object pair {
    def apply[X,Y,Z] (op: (X,Y) => Z, xy:(X,Y))   = xy match{ case(x,y) => op(x,y) }
    
    def _multiply[T <: Double] (x: T, y: T)   :Double = x*y
    def _equal   [T <: Double] (x: T, y: T)   :Boolean = x == y
    
    def multiply[T <: Double](xy: (T,T)): Double  = apply(_multiply, xy)
    def equal[T <: Double](xy: (T,T))   : Boolean = apply(_equal, xy)
    
  };
  
  /**
   * Dot product between vectors
   */
  def dot( x: Array[Double], y: Array[Double]) : Double =  x.zip(y).map(pair.multiply).sum
    
  def eq[T <:Double](x: Array[T], y:Array[T]) : Boolean =  x.zip(y).map(pair.equal).reduce(_ && _)
    
  /**
   * Unoptimized multiply two dimensional matrix:
   *  { A \in R^m*n with vector v \in R^n  returning vector in R^m}
   */
  def multiply(A: Array[Array[Double]], x: Array[Double]) : Array[Double] = A map { r =>  dot(x,r) }
  
  /**
   *  
   */
  def main(args: Array[String]) = {
    
    val I = Array(Array(1.0 , 0   , 0),
                  Array(0   , 1.0 , 0),
                  Array(0   , 0   , 1.0))
                  
    val x = Array(1.0, 2.0, 3.0)
    assert(eq(x, multiply(I,x)) == true)
    println(multiply(I,x).toList)
    
  }
  
}