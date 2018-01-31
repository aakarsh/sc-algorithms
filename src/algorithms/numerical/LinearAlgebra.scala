package algorithms.numerical

object LinearAlgebra {
  
  object pair {
    def apply[X,Y,Z] (op: (X,Y) => Z, xy:(X,Y))   = op(xy._1,xy._2)
    
    def _multiply[T <: Double] (x: T, y: T)   :Double  = x * y
    def _equal   [T <: Double] (x: T, y: T)   :Boolean = x == y
    
    def multiply[T <: Double](xy: (T,T)): Double  = apply(_multiply, xy)
    def equal[T <: Double](xy: (T,T))   : Boolean = apply(_equal   , xy)
  }
  
  /**
   * Dot product between vectors
   */  
  def dot( x: Array[Double], y: Array[Double]) : Double  =  x.zip(y).map(pair.multiply).reduce(_ + _)
  
  def eq[T <:Double](x: Array[T], y:Array[T]) :  Boolean =  x.zip(y).map(pair.equal).reduce(_ && _)
  
  implicit class Matrix(m: Array[Array[Double]]) {
  
  val nrows = m.length
  val ncols = m(0).length
  
  def swap_rows(r1:Int , r2:Int) :Unit = {
    def swap(c:Int) = swap_entry((r1,c), (r2,c))
    
    (0 until ncols) foreach swap        
  }
  
  def swap_cols(c1: Int, c2:Int): Unit = {    
    def swap(r:Int) = swap_entry((r,c1), (r,c2))
      
    (0 until nrows) foreach swap    
  }
  
  def swap_entry( e1:(Int,Int), e2:(Int,Int)): Unit = {
    val t    = m(e1._1)(e1._2)
    m(e1._1)(e1._2)  = m(e2._1)(e2._2) 
    m(e2._1)(e2._2)  = t
  }
  
  def scale_row( r:Int , scale:Double) : Unit =  
    m(r) = m(r).map(v => scale * v)
  
  /**
   * Convert Ax = b into upper triangular form Ux = y
   * 
   * Assumes A is invertible. 
   */
  def forward_subsitute(b: Array[Double]) : (Matrix, Array[Double]) = {
    val U = m.clone()
    val y = b.clone()
    
    for(pivot <- 0 until nrows) {
      val p = pivot
      
      // scale row of pivot row to make the pivot entry equal to 1
      val scale = 1/U(p)(p)
      y(p) /= scale 
      U(p) = U(p) map ( _ /scale )
      
      // eliminate pivot from future rows below pivot entry
      for(r <- (p + 1) until nrows) {
        val scale = -U(r)(p) // entry in current row of pivot column
        // eliminate in y
        y(r) = y(r) + y(p) * scale
        for( c <- p until ncols) {
          U(r)(c) = U(r)(c) + scale*U(p)(c)
        }
      }
    }
    (U,y)
  }
  
  def backward_subsitute(y: Array[Double]) : Array[Double] = {
    val x = y.clone
    for(pivot <- (nrows - 1) to 0 by -1) { // go back by row
      println("pivot :" + pivot)
    }
    x
  }
  
  /**
   * Unoptimized multiply two dimensional matrix:
   *  { A \in R^(m*n) with vector v \in R^n  returning vector in R^m }
   */
  def multiply(x: Array[Double]) : Array[Double] = 
    m map { r =>  dot(x,r) }  
  
  override def toString : String  = {
      val nrows = m.length
      val ncols = m(0).length
      val s = new StringBuilder
      for( i <- 0 until nrows) {
        
        for(j <- 0 until ncols) {
          s ++= m(i)(j).formatted("%5.2f") +" "    
        }
        s ++= "\n"
      }          
      s.toString()
    }
  }

  /**
   *  
   */
  def main(args: Array[String]) = {    
    val I:Matrix = Array(Array(1.0 , 0.0   , 0.0),
                         Array(0.0 , 1.0   , 0.0),
                         Array(0.0 , 0.0   , 1.0))
                  
    val x = Array(1.0, 2.0, 3.0)
    
    assert(eq(x, I.multiply(x)) == true)

    println(I.multiply(x).toList)
        
    val b = x.clone()
    val Ub = I.forward_subsitute( b)
    
    println("After forward : ")
    println(Ub._1)
    println(Ub._2.toList)
    
  } 
}