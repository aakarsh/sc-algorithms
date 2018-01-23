package algorithms

import scala.util.control.Breaks._
import dp.Numerics._  
import Sets._

object Main {
    
  def main(args: Array[String]): Unit = {
    
    val elements = Array(1,2,3,4)
    
    println("Subsets of: ["+elements.mkString(",")+"]")
    
    for(subset <- subsets(elements)) 
      println("["+subset.mkString(",")+"]")
    
  }   
  
}