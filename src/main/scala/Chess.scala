import collection.mutable.Map
object Chess extends App {
  var board:Map[String,String] = Map()
    def checkBoardPos(k:Int):String={
      k match{
        case 1 => "A"
        case 2 => "B"
        case 3 => "C"
        case 4 => "D"
        case 5 => "E"
        case 6 => "F"
        case 7 =>  "G"
        case 8 => "H"
        case _ => "ll"
      }
    }

  def setupBoard(size:Int)={
    def makeBoard(size:Int)={
      var boardPos = ""
      for (k <- 1 to size){
        boardPos = checkBoardPos(k)
        for (p <- 1 to size){
          board += ((boardPos+p)->"empty")
        }
      }
      println(board.keys.size)
    } //make the board

    def fillBoard(size:Int,White:Boolean)={
      var Pieces:List[String] = List("K","Q","R","k","B","P")
      var boardPos = ""
      var piece = ""
      var a = size
      var b = size-1
      var c = -1
      var pieceColor = "B"
      //White
      if (White){
        a = 1
        b = 2
        c = 1
        pieceColor = "W"
      }
      for (k <- a to b by c) {
        boardPos = checkBoardPos(k)
        for (p <- 1 to size) {
          if (k == a){
            p match{
              case 1|8 => piece ="R"
              case 2|7 => piece = "k"
              case 3|6 => piece = "B"
              case 4 => piece = "K"
              case 5 => piece = "Q"
            }
          }
          else{
            piece = "P"
          }
          board(boardPos+p) = pieceColor+piece
        }
      }
    } //fill the board with the pieces

    makeBoard(size)
    fillBoard(size,true)
    fillBoard(size,false)
  } //Sets up and fills the board

  setupBoard(8)
  board.foreach(elem => println(elem))

  def boardMovement(Position:String,piece:String,PieceType:PieceInformation)={
    var currentPosition = Position.toList //1st element is A, 2nd is 1
    //var boardPosInt = checkBoardPosToInt(currentPosition.head)
    var theIntPos = currentPosition(1).toInt

    def checkBoardPosToInt(k:Char):Int={
      k.toString match {
        case "A" => 1
        case "B" => 2
        case "C" => 3
        case "D" => 4
        case "E" => 5
        case "F" => 6
        case "G" => 7
        case "H" => 8
      }
    }
    var boardPosInt = checkBoardPosToInt(currentPosition.head)

    def Left()={
      theIntPos -= 1
    }
    def Right()={
      theIntPos += 1
    }
    def Up()={
      boardPosInt -= 1
    }
    def Down()={
      boardPosInt += 1
    }
    def Diagonal(theCase:String)={
      theCase match{
        case "LU" => Left()
          Up()
        case "RU" => Right()
          Up()
        case "LD" => Left()
          Down()
        case "RD" => Right()
          Down()
      }

    }

    var choice = ""
    if(PieceType == "Knight"){

    }
    else{

    }
    for (k <- 0 to PieceType.moveDistance){
      choice = readLine(s"Choose Where to move: ${PieceType.possibleMoves.foreach(direction => print(direction+","))}")
    }

    var boardPos = checkBoardPos(boardPosInt)
    var newPosition = boardPos+theIntPos
    board(Position) = "empty"
    board(newPosition) = piece


  } //Move a piece of your choice


}

abstract class PieceInformation{
  var theType:String
  var possibleMoves:List[String] = List("U","L","D","R","UL","UR","DL","DR") //i.e = "L","R","LU","RU")
  var moveDistance:Int = 7 //i.e 1,3,7(max)
  def specialMove()={} //Pawn move 2 for first move, knight move

}

class Rook extends PieceInformation{
  var theType = "Rook"
  override var possibleMoves = List("U","L","D","R")
}

class Bishop extends PieceInformation{
  var theType = "Bishop"
  override var possibleMoves = List("UL","UR","DL","DR")
}

class King extends PieceInformation{
  var theType = "King"
  override var moveDistance = 1
}

class Queen extends PieceInformation{
  var theType = "Queen"
}

class Pawn extends PieceInformation{
  var theType = "Pawn"
  override var possibleMoves = List("U")
  override var moveDistance = 1
  var firstMove = true
  override def specialMove()= {
    if (firstMove) {
      moveDistance = 2
    }
  }
}

class Knight extends PieceInformation{
  var theType = "Knight"
  override var possibleMoves = List("U","L","D","R")
  override var moveDistance = 3
}