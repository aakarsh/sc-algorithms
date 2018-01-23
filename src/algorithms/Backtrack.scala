package algorithms

import scala.util.control.Breaks._

object Backtrack {  
  
  def backtrack[T, S](input    : Array[T], 
                      options  : Array[S], 
                      position : Int,
                      terminal : (Array[T], Array[S], Int) => Boolean,
                      generator: (Array[T], Array[S], Int) => List[S],
                      processor: (Array[T], List[S], Int)  => Boolean): Boolean = {
    
    if(terminal(input,options,position)) {
      processor(input,options.toList,position);      
      return false;
    }
    
    val generated_options = generator(input,options,position)
    
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
  
}