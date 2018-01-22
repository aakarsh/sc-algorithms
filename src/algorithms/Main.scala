package algorithms
import scala.util.control.Breaks._

object Main {
    
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
  
  def subsets[T] (elements: Array[T]) : List[List[T]] = {
    
    var results: List[List[T]] = List[List[T]]()
    
    def terminal(input:Array[T], options: Array[Boolean], position: Int): Boolean = {
       return options.length == position
    }
    
    def generator(input:Array[T], options: Array[Boolean], position: Int) : List[Boolean] = {
      return List(true,false)
    }
    
    def processor(input:Array[T], options: List[Boolean], position: Int) : Boolean = {
      
      def getElement(include:Boolean, index:Int) : Option[T] =        
        return if(include) Some(input(index)) else None
      
      def optionalElement(e: Option[T]) : List[T] = e  match {
          case Some(e) => List(e)
          case None => Nil
        }
      
      val subset: List[T]= 
        options.zipWithIndex
          .map(z => getElement(z._1, z._2))
          .flatMap(optionalElement).toList
      
      results = subset::results
      return false
    }
    
    backtrack(elements,
        new Array[Boolean](elements.length), 0, 
        terminal, generator, processor)
    
    return results
  }
  
  def backtrack[T, S](input    : Array[T], 
                      options  : Array[S], 
                      position : Int,
                      terminal : (Array[T], Array[S], Int) => Boolean,
                      generator: (Array[T], Array[S], Int) => List[S],
                      processor: (Array[T], List[S], Int) => Boolean): Boolean = {
    
    if(terminal(input,options,position)) {
      processor(input,options.toList,position);      
      return false;
    }
    
    val generated_options = generator(input,options,position);
    breakable {
      for(option <- generated_options) {
        options(position) = option;
        val shortCircuit = backtrack(input, options, position+1, terminal, generator, processor);
      
        if(shortCircuit) 
          break;        
        }
     }
    return false;    
  }
  
  def main(args: Array[String]): Unit = {
    
    val elements = Array(1,2,3,4);
    println("Subsets of :"+elements.toList)
    
    for(subset <- subsets(elements)) {
      println("["+subset.mkString(",")+"]")
    }
    /**
	   * for(i <- Range(0, 1000)) {
     * 	printf("%10d %-20d \n", i , fib(i))
	   * }
    	 */
  }   
  
}