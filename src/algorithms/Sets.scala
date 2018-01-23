package algorithms

import Backtrack._

object Sets {

  def subsets[T] (elements: Array[T]) : List[List[T]] = {
    
    var results: List[List[T]] = List[List[T]]()
    
    def terminal(input: Array[T], options: Array[Boolean], position: Int): Boolean =
       return options.length == position

    
    def generator(input:Array[T], options: Array[Boolean], position: Int) : List[Boolean] = 
      return List(true,false)
        
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
}