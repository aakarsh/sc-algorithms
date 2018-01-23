package algorithms

import Backtrack._
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.HashSet

object Sets {
  
  def permutations(from:Int , till:Int): List[List[Int]] = {  
    
    var results: ListBuffer[List[Int]] = ListBuffer[List[Int]]()
      
    val termCond : TerminalCond [Int,Int]  =
      (_,options,position) => options.length == position    
    
    val generator: GeneratorFunc[Int,Int]  = (input,options,position) => {      
      val emptySet = new HashSet[Int]()
      val selected  = options.take(position).foldLeft(emptySet)(_ + _)
      input.filter(!selected.contains(_)).toList
    }
    
    val process  : ProcessorFunc[Int,Int] = (_,options,_) => {
      results.append(options.toList)
      false
    }
    
    backtrack((from until till).toArray, termCond, generator, process)
    
    results.toList
  }
  
  def permute[T] (elements: Array[T]) : List[List[T]] = 
    permutations(0,elements.length).map(indices => indices.map(elements(_))).toList  

  def subsets[T] (elements: Array[T]) : List[List[T]] = {    
    var results: ListBuffer[List[T]] = ListBuffer[List[T]]()
    
    def filterSelected(input:Array[T], options: List[Boolean]) : List[T] =
        options.zipWithIndex.filter(_._1 != false).map(x => input(x._2)).toList    
    
    val termCond : TerminalCond [T,Boolean]  = (_,options,position) => options.length == position
    val generator: GeneratorFunc[T,Boolean] = (_,_,_) => List(false,true)
    val process  : ProcessorFunc[T,Boolean] = (input,options,_) => { 
      results.append(filterSelected(input,options))
      false
    }
    
    backtrack(elements, termCond, generator, process)
    
    return results.toList
  }
}