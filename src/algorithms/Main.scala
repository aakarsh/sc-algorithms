package algorithms

import scala.util.control.Breaks._
import dp.Numerics._  
import Sets._
import Graphs._

object Main {  
    
  def main(args: Array[String]): Unit = {
    
    val elements = Array(1,2,3,4)
    
    println("Subsets of: ["+elements.mkString(",")+"]")
    
    for(subset <- subsets(elements)) 
      println("["+subset.mkString(",")+"]")
    
    
    val letters = Array('a','b','c')
    println("Permutation of: ["+letters.mkString(",")+"]")
    
    for(perm <- permute(letters))
      println("["+perm.mkString(",")+"]")
      
     
     val g:Array[Array[Int]] = 
       Array(Array(0, 1, 0, 1 ,0),
             Array(1, 0, 1, 0 ,0),
             Array(0, 1, 0, 0 ,0),
             Array(1, 0, 0, 0 ,0),
             Array(0, 0, 1, 0 ,0))
        
      val src = 0
      val dest = 3
      
      printf("Paths from %d to %d:\n" , src,dest)
      for (path <- paths(g, src,dest)) 
          println(path)
    
  }   
  
}