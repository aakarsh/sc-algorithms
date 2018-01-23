package algorithms

import Backtrack._
import scala.collection.mutable.ListBuffer

object Sets {

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