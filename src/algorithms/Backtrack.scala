package algorithms

import scala.util.control.Breaks._
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

object Backtrack {  
  
  // reached leaf node
  type TerminalCond[T,S]  = (Array[T], Array[S], Int) => Boolean
  
  // perform action on leaf node 
  type ProcessorFunc[T,S] = (Array[T], List[S], Int)  => Boolean
  
  // input array , state vector with valid values till position, output: list of possible next states 
  type GeneratorFunc[T,S] = (Array[T], Array[S], Int) => List[S]
 
  /**
   * Backtrack will iterate through the input array from left to right.
   *  
   * After each progress it will ask the generator to produce a list of possible
   * next states based on input , previous states and position input observed.
   * 
   * When a terminal state is reached the processor function will take the state vector
   * and perform some operation on the constructed state vector and the input provided.
   * 
   */
  def backtrack[T,S](input    : Array[T],                     
                     terminal : TerminalCond[T,S],
                     generator: GeneratorFunc[T,S],
                     processor: ProcessorFunc[T,S]) (implicit m: ClassTag[S]): Boolean = { 

    return backtrack_r(input, new Array[S](input.length), 0,
                       terminal, generator,processor);
  }
  
  /**
   * Generalized back tracking algorithm. 
   */
  def backtrack_r[T, S](input    : Array[T], 
                        options  : Array[S], 
                        position : Int,
                        terminal : (Array[T], Array[S], Int) => Boolean,
                        generator: (Array[T], Array[S], Int) => List[S],
                        processor: (Array[T], List[S], Int)  => Boolean): Boolean = {
    
    if(terminal(input, options, position)) {      
      return processor(input, options.toList, position)      
    }
        
    breakable {
      for(option <- generator(input,options,position)) {        
        options(position) = option // save option and proceed backtracking
        val shortCircuit = backtrack_r(input, options, position + 1, terminal, generator, processor)
      
        if(shortCircuit) 
          break;        
        }
     }
    return false
  }
  
}