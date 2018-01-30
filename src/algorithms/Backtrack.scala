package algorithms

import scala.util.control.Breaks._
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

object Backtrack {  
  
  // reached leaf node  
  type TerminalCond[T,S]  = (Array[T], List[S]) => Boolean
  
  // perform action on leaf node 
  type ProcessorFunc[T,S] = (Array[T], List[S])  => Boolean
  
  // input array , state vector with valid values till position, output: list of possible next states 
  type GeneratorFunc[T,S] = (Array[T], List[S]) => List[S]
 
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
  def backtrack[T, S](  input    : Array[T], 
                        states   : List[S],                       
                        terminal : TerminalCond[T,S],
                        generator: GeneratorFunc[T,S],
                        process  : ProcessorFunc[T,S]): Boolean = {
    
    if(terminal(input, states)) {      
      return process(input, states.reverse)      
    }
        
    breakable {
      for(state <- generator(input,states)) {
        
        val shortCircuit = 
          backtrack(input, state::states, terminal, generator, process)
      
        if(shortCircuit) 
          break;        
        }
     }
    return false
  }
  
}