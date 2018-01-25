package algorithms

import Backtrack._
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.HashSet

object Sets {
  
  /**
   * Generates a set of all permutations of integers from the `from`
   * to the `till` index.
   */
  def permutations(from:Int , till:Int): List[List[Int]] = {  
    
    var results: ListBuffer[List[Int]] = ListBuffer[List[Int]]()
      
    val termCond : TerminalCond [Int,Int]  =
      (input,states) => states.length == input.length    
    
    val generator: GeneratorFunc[Int,Int]  = (input,states) =>       
      input.filter(!states.toSet.contains(_)).toList
        
    val process  : ProcessorFunc[Int,Int] = (_,states) => {
      results.append(states.toList)
      false
    }
    
    backtrack((from until till).toArray,List[Int](), termCond, generator, process)
    
    results.toList
  }
  
  def permute[T] (elements: Array[T]) : List[List[T]] = 
    permutations(0,elements.length).map(indices => indices.map(elements(_))).toList  

    
  /**
   * Returns the set of all subsets of of elements of array. 
   */
  def subsets[T] (elements: Array[T]) : List[List[T]] = {
    
    var results: ListBuffer[List[T]] = ListBuffer[List[T]]()
    
    def filterSelected(input:Array[T], options: List[Boolean]) : List[T] =
        options.zipWithIndex.filter(_._1 != false).map(x => input(x._2)).toList    
    
    val termCond : TerminalCond [T,Boolean]  = 
      (input,states) => states.length == input.length
    
    val generator: GeneratorFunc[T,Boolean] = (_,_) => List(false,true)
    
    val process  : ProcessorFunc[T,Boolean] = (input,states) => { 
      results.append(filterSelected(input,states))
      false
    }
    
    backtrack(elements,List[Boolean]() ,termCond, generator, process)
    
    return results.toList
  }
  
}