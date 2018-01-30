package algorithms

import scala.collection.mutable.ListBuffer
import Backtrack._
import scala.collection.immutable.HashSet

object Graphs {
  
  val debug = false
  
  trait Graph {    
    def vertices: Seq[Int] 
    def neighbours(vertex:Int): Seq[Int]
  }
  
  implicit class AdjGraph(g: Array[Array[Int]]) extends Graph{
    
    def vertices = (0 until g.length)
    
    def neighbours(v:Int): Seq[Int] =  
      g(v).toList
      .zipWithIndex
      .filter({ case (hasEdge: Int, index: Int) => hasEdge == 1 }).map(_._2)      
  }
 
  def paths(graph: Graph, src: Int, dest: Int) : List[List[Int]] = {
        
    var results: ListBuffer[List[Int]] = ListBuffer[List[Int]]()
    
    type StateType = Int
    type InputType = Array[Int]
    
    val termCond : TerminalCond [InputType,StateType]  =
      (_,states) => {
        if(states.length > 0 && states.head == dest) {
          if(debug)
            println("Reached destination "+dest);
          true
        } else 
          false
      }
    
    val generator: GeneratorFunc[InputType,StateType]  = (_,states) => {
      
      val prev: Int = states.head
        
      if(debug)
        printf("Visiting : %d, prev_states : %s \n", prev,states.mkString(","))
            
      val visited: Set[StateType] = states.toSet
        
      if(debug)
        printf("Visited: %s \n", visited.mkString(","))
        
      val next = graph.neighbours(prev).filter(!visited.contains(_))

      if(debug)
        printf("Neighbours of %d : with Neighbours[%s]\n",prev, next.mkString(","))
        
      next.toList      
    }
    
    val process: ProcessorFunc[InputType,StateType] = (_,path) => {
      results.append(path.toList)
      false
    }
    
    backtrack(null,List[Int](src), termCond, generator, process)
    
    results.toList
  }  
}