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
  
  object Matrix {
    
    def polynomial_regression(xy:List[(Double, Double)]): List[Double] = {      
      val n:Int = xy.size
      
      val y = new Array[Double](n)      
      val m = new Array[Array[Double]](n)
      
      var l = xy
      for(i <- 0 until n) {
        val (x_i,y_i) = l.head
        
        for(j<- 0 until n) {
          if(j == 0)
            m(i) = new Array[Double](n)
            
          m(i)(j) = Math.pow(x_i,j)
        }
        
        y(i) = y_i
        l = l.tail
      }
      
      val M = new Matrix(m)      
      M.solve(y).toList
    }
    
    def regression(xy: List[(List[Double], Double)]) : List[Double] = {
      val n = xy.size  // assuming square.
      
      val y = new Array[Double](n)
      val m = new Array[Array[Double]](n)
      
      var i = 0
      for( (xs , f) <- xy) {
        
        var j = 0
        
        for(x <- xs) {
          if(j == 0)
            m(i) = new Array[Double](n)
          m(i)(j) = x
          
          j+=1
        }
        
        y(i) = f
        i+=1
      }
      
      val M = new Matrix(m)
      M.solve(y).toList
    }
    
     def I(size:Int):Matrix = identity(size)
     
     def identity(size:Int):Matrix = {
       val v = new Array[Double](size)
       for (i <- 0 until size) {
         v(i) = 1
       }
       diagonal(v)
     }
     
     def diagonal(diagonal:Array[Double]) : Matrix = {
        val m = new Array[Array[Double]](diagonal.length)
        for(i <- 0 until diagonal.length) {
          m(i) = new Array[Double](diagonal.length)
          m(i)(i) = diagonal(i)
        }     
        m
     }       
  }
  
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
  
  def solve(b:Array[Double]) : Array[Double] = {
    var retval = Array[Double](b.size)
    forward_substitute(b) match {
      case (u:Matrix,y: Array[Double]) => {
        retval = u.backward_substitute(y)
      }
    }
    retval
  }
  /**
   * Convert Ax = b into upper triangular form Ux = y
   * 
   * Assumes A is invertible. 
   */
  def forward_substitute(b: Array[Double]): (Matrix, Array[Double]) = {
    
    val U = m.clone()
    val y = b.clone()
    
    for(pivot <- 0 until nrows) {
      val p = pivot
      
      // Scale row of pivot row to make the pivot entry equal to 1
      val scale = 1/U(p)(p)
      y(p) *= scale 
      U(p) = U(p) map ( _ * scale )
      
      // Eliminate pivot from future rows below pivot entry
      for(r <- (p + 1) until nrows) {
        val scale = -U(r)(p) // entry in current row of pivot column
        // eliminate in y
        y(r) = y(r) + y(p) * scale
        for( c <- p until ncols) {
          U(r)(c) = U(r)(c) + scale * U(p)(c)
        }
      }
    }
    (U,y)
  }
  
  def backward_substitute(y: Array[Double]): Array[Double] = {
    val x = y.clone
    for( pivot <- (nrows - 1) to 0 by -1 ) { // go back by row.
        //println("pivot:"+pivot)
        //println("x:\n"+x.toList)
        for( r <- (0 to (pivot-1))) {
          val p = pivot
          x(r) = x(r) - (m(r)(p) / m(p)(p)) * x(p)
        }
    }
    x
  }
  
  var lu_cache: Option[Array[Array[Double]]] = None
  
  def lu_factorization : Matrix = {
    if(lu_cache.isEmpty) {
       val a = m.clone()
       for( p <- 0 until ncols) {
         for( r <- (p + 1) until nrows) {
           
           val scale = -a(r)(p)/a(p)(p) // does not account f
           a(r)(p)   = -scale // set r > b, lower part of matrix
           
           for( c <- (p + 1) until ncols ) {
             a(r)(c) += scale * a(p)(c)
           }           
         }
       }
       
       lu_cache = Some(a)       
    }
    lu_cache.get
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
      s.toString
    }
  } 

  /**
   *  
   */
  def main(args: Array[String]) = {    
    
    val I: Matrix = Matrix.identity(3)
                  
    val x = Array(1.0, 2.0, 3.0)
    
    assert(eq(x, I.multiply(x)) == true)

    println(I.multiply(x).toList)

    val b = x.clone()
    val Ub = I.forward_substitute( b)
    
    val U = Ub._1
    val y = Ub._2
    
    assert(eq(x, U.backward_substitute(y)) == true)
    
    println("After forward:")
    println(U)
    println(y.toList)
    
    println("LU :")
    
    println(Matrix.diagonal(Array(1.0,2.0,3.0)).lu_factorization)
    
    val v: Matrix = Array(Array(1.0, 1.0, -2.0),
                          Array(0.0, 1.0, -1.0),
                          Array(3.0, -1.0, 1.0))

    println(v.lu_factorization)
    
    val r = Matrix.regression(List((List(1,2),1) , 
                                   (List(2,3),2) ,
                                   (List(3,4),3)))
                        
    println(r)
    
    val pr = Matrix.polynomial_regression(List((-1,1),(0,-1),(2,7)))
    println(pr)
    
  }
  
}