package algorithms
import Backtrack._
import scala.collection.mutable.ListBuffer

object Sudoku {
  
  class Position(x:Int, y:Int, value:Int) 
  
  implicit class Sudoku(board: Array[Array[Int]]) {

    def border: String = ("+" + (" - " * 3)) * 3+ "+\n"
    
    /**
     * Pick the next open square.
     */
    def nextPosition: Position = {
        new Position(0,0,-1)
    }
    
    /**
     * Provide a list of possible values for the given board. 
     */
    def possibleValues(x: Int, y:Int): List[Int] = {
      val rowValues : List[Int] = board(x).toList.filter(_ > 0)
      val colValues : List[Int] = (0 to board.length).map(board(_)(y)).toList.filter(_ > 0)      
      val secValues = sectorValues(sector(x,y))
      val usedValues = List(rowValues, colValues, secValues).flatten.toSet
      (0 until 9).filter(! usedValues.contains(_)).toList
    }
    
    /**
     * Returns all the values of a sector
     */
    def sectorValues(sector: Int) : List[Int] = {      
      var retval: ListBuffer[Int] = ListBuffer[Int]()
      for { x <- 0 to 2 
            y <- 0 to 2 } {
        retval += board(x + (sector/3))(y + sector/3)
      }
      retval.filter(_ > 0).toList
    }
    
    /**
     * Returns the sector inside the sudoku puzzle.
     */
    def sector(x:Int , y: Int) : Int = {
      return (x/3, y/3) match {
          case (0,0) => 0
          case (0,1) => 1
          case (0,2) => 2
          case (1,0) => 3
          case (1,1) => 4
          case (1,2) => 5
          case (2,0) => 6
          case (2,1) => 7
          case (2,2) => 8
          case _     => -1
      }
    }
    
    def solve(): Unit = {      
      
      //val term:TermCond[Sudoku,Int] = (
      
      val g: GeneratorFunc[Array[Int], List[Position]] = (in, states) => {        
        List[List[Position]]()
      }
      
      //backtrack(null,term,gen,proc)
      
    }    
    
    override def toString : String = {      
      var str:StringBuilder = new StringBuilder
      
      for { i <- 0 until 9
            j <- 0 until 9 } 
      {
      
        if(i % 3 == 0 && j == 0)
          str ++= border
          
        if(j % 3 == 0)
          str ++= "|"
          
        if(board(i)(j) > 0) {
          str ++= " " + board(i)(j)+" "
	      } else {
          str ++= " - "  
        }
	
        if(j == 8)
          str ++= "|\n"
      }
      
      str ++= border
      return str.toString()
    }
  }
  
  def parse(numbers: String, boardSize: Int = 9) : Array[Array[Int]] = {

    val board = new Array[Array[Int]](boardSize)
    
    for(i<- 0 until boardSize)
      board(i) = new Array[Int](boardSize)
      
    for{ i <- 0 until boardSize 
         j <- 0 until boardSize } 
        board(i)(j) = -1
      
    val values = numbers.split("\\s+")
    var pos = 0
    
    for{ i <- 0 until 9
         j <- 0 until 9 } {
           
      if(!(values(pos) == "-"))
         board(i)(j) = values(pos).toInt
         
      pos +=1
    }    
         
    return board        
  }
}