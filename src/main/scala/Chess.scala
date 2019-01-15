import collection.mutable.Map
object Chess extends App {
  var board:Map[String,String] = Map()
  var BlackList:List[String] = List()
  var WhiteList:List[String] = List()
  var fullWhiteList:List[String] = List()
  var fullBlackList:List[String] = List()

  ///CREATE BOARD START
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

    def fillBoard(size:Int,White:Boolean):List[String]={
      var Pieces:List[String] = List("K","Q","R","k","B","P")
      var pieceList:List[String] = List()
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
          pieceList ::= piece
          board(boardPos+p) = pieceColor+piece
        }
      }

      return pieceList
    } //fill the board with the pieces

    makeBoard(size)
    fullWhiteList = fillBoard(size,true)
    WhiteList = fullWhiteList.distinct
    fullBlackList = fillBoard(size,false)
    BlackList = fullWhiteList.distinct
  } //Sets up and fills the board

  ///CREATE BOARD END
  ///
  ///

//  board.foreach(elem => println(elem))
//  println(WhiteList)
//  println(BlackList)

  ///CHOOSE AND MOVE START
  //Choose a piece of your choice

  def choosePiece(White:Boolean = true):Tuple2[String,PieceInformation]={
    def getPiece():PieceInformation={
      var chosenPiece = readLine(createChoiceString(WhiteList,"What piece would you like to choose:"))
      chosenPiece match{
        case "P"|"p" => new Pawn()
        case "R"|"r" => new Rook()
        case "k" => new Knight()
        case "B"|"b" => new Bishop()
        case "Q"|"q" => new Queen()
        case "K" => new King()
      }
    }
    var chosenPiece = getPiece()

    def getBoardPosition(chosenPiece:PieceInformation,White:Boolean):List[String]={
      var w = "B"
      var possiblePositions:List[String] = List()
      if (White){
        w = "W"
      }
      println(w+chosenPiece.theType.substring(0,1))


      board.foreach((elem:(String,String)) =>
        if (elem._2 == w+chosenPiece.theType.substring(0,1)){
          possiblePositions ::= (elem._1)} )
      return possiblePositions
    }
    var thePositions = getBoardPosition(chosenPiece,White)
    var choosePiecePosition = ""
    while(!thePositions.contains(choosePiecePosition)){
      choosePiecePosition = readLine(createChoiceString(thePositions,"At which position would you like to move from: "))
    }
    return(choosePiecePosition,chosenPiece)
  }


  def makeInt(k:Char):Int={
      k match{
        case '1' => 1
        case '2' => 2
        case '3' => 3
        case '4' => 4
        case '5' => 5
        case '6' => 6
        case '7' => 7
        case '8' => 8
      }
    }

  def boardMovement(Position:String,PieceType:PieceInformation)={
    var currentPosition = Position.toList //1st element is A, 2nd is 1
    //Get white or black
    var WoB = board(Position).toList(0)
    var theIntPos = makeInt(currentPosition(1))
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
    def Foward()={
      WoB match{
        case 'W' => boardPosInt += 1
        case 'B' => boardPosInt -= 1
      }
    }
    def Backward()={
      WoB match{
        case 'W' => boardPosInt -= 1
        case 'B' => boardPosInt += 1
      }
    }
    def Diagonal(theCase:String)={
      theCase.toUpperCase() match{
        case "FL" => Left()
          Foward()
        case "FR" => Right()
          Foward()
        case "BL" => Left()
          Backward()
        case "BR" => Right()
          Backward()
      }

    }


    def chooseMovement(choice:String)={
      choice.substring(0,2).toLowerCase() match{
        case "le" => Left()
        case "ri" => Right()
        case "fo" => Foward()
        case "ba" => Backward()
        case _ => Diagonal(choice)
      }
    }
    def choosePossibleMoves(possibleMoves:List[String]):String={
      var choice = ""
      while (!possibleMoves.contains(choice)){
        choice = readLine(createChoiceString(possibleMoves,"Choose a direction:")).toUpperCase()
      }
      return choice.substring(0,2)
    }
    def checkCollision(Position:String):Boolean={
      if (board(Position) != "empty"){
        return true
      }
      return false
    }

    var boardPos = checkBoardPos(boardPosInt)
    var newPosition = boardPos+theIntPos
    var collided = false

    if (PieceType.theType == "Pawn"){//PAWN SPECIAL RULES
      chooseMovement("fl")
      boardPos = checkBoardPos(boardPosInt)
      var FLcollide = checkCollision(checkBoardPos(boardPosInt)+theIntPos)
      println("Collide?"+FLcollide)
      if (FLcollide){
        PieceType.possibleMoves ::= "FL"
      }
      chooseMovement("br")
      chooseMovement("fr")
      boardPos = checkBoardPos(boardPosInt)
      var FRcollide = checkCollision(checkBoardPos(boardPosInt)+theIntPos)
      println("Collide2?"+FRcollide)
      if (FRcollide){
        PieceType.possibleMoves ::= "FR"
      }
      chooseMovement("bl")

    }
    var choice = choosePossibleMoves(PieceType.possibleMoves)

    if(PieceType.theType == "knight"){
      chooseMovement(choice)
      chooseMovement(choice)
      choice.substring(0,1).toUpperCase() match{
        case "L"|"R" => chooseMovement(choosePossibleMoves(List("FORWARD","BACKWARD")))
        case "F"|"B" => chooseMovement(choosePossibleMoves(List("LEFT","RIGHT")))
      }
      boardPos = checkBoardPos(boardPosInt)
      newPosition = boardPos+theIntPos
      collided =checkCollision(newPosition)
    }
    else{
      var moveChoice = PieceType.moveDistance
      if (PieceType.theType == "Pawn" &&(boardPosInt == 2 || boardPosInt == 7)){//So starting positions
        PieceType.moveDistance = 2
      }
      if (PieceType.moveDistance > 1){
        println(s"You can move ${PieceType.moveDistance} squares. How much do you want to move?")
        moveChoice = readInt()
      }

      for (k <- 1 to moveChoice){
        chooseMovement(choice)

        if (!collided){
          collided = checkCollision(checkBoardPos(boardPosInt)+theIntPos)
          boardPos = checkBoardPos(boardPosInt)
          newPosition = boardPos+theIntPos
        }
      }
    }
    //Get Collisions
    var allMoveList:List[String] = List()
    var collidedList: Map[String, String] = Map()

    //Complex Collision function
//    def getCollisions(Position: String, PieceType: PieceInformation)= {
//
//      PieceType.possibleMoves.foreach(move => {
//        var moveList: List[String] = List()
//        var collided = false
//        var k = 1
//        var boardPosInt = checkBoardPosToInt(currentPosition.head)
//        var theIntPos = makeInt(currentPosition(1))
//        while (!collided && k <= PieceType.moveDistance) {
//          chooseMovement(move)
//          var boardPos2 = checkBoardPos(boardPosInt)
//          var newPosition2 = boardPos2 + theIntPos
//          if (board(newPosition2) != "empty") {
//            collided = true
//          }
//          moveList ::= newPosition2
//          k += 1
//        }
//        if (PieceType.moveDistance > 1){
//          allMoveList = moveList.tail
//          collidedList += (move -> moveList.head)
//        }
//        else{
//          if (board(moveList.head) != "empty"){
//            collidedList += (move -> moveList.head)
//          }
//          else{
//            allMoveList = moveList
//          }
//
//
//        }
//
//      })
//      println("Movements =>"+allMoveList)
//      println("Collisions =>"+collidedList)
//    }
    ///
    //getCollisions(Position, PieceType)

    //If piece collides, it just removes the other piece from the board
//    if (board(newPosition) != "empty"){
//      if (WoB == 'W'){
//        fullBlackList = fullBlackList diff List(board(newPosition))
//      }
//    }

    board(newPosition) = board(Position)
    if (newPosition != Position){
      board(Position) = "empty"
    }

  } //Move a piece of your choice


  def MainGame()={
    //Setup
    setupBoard(8)
    var choiceList:List[String]= List("Move","Check Pieces")
    var end = false
    var White = true
    //Main Loop
    def checkPieces()={
      println("Your pieces: "+WhiteList)
      println("Enemy Pieces: "+BlackList)
    }
    def chooseAction()={
      var choice = (readLine(createChoiceString(choiceList,"Choose an Action: "))+ " ").toLowerCase.substring(0,1)
      choice match{
        case "m" =>
          var piece = choosePiece(White)
          boardMovement(piece._1,piece._2)
        case "c" => checkPieces()
      }
    }
    def changeWhite(White:Boolean):Boolean={
      White match{
        case true => false
        case false => true
      }
    }

    while(!end){//Playing both sides right now
      println("WHITE: "+White)
      chooseAction()
      White = changeWhite(White)
    }
  }
  MainGame()

  ///CHOOSE AND MOVE END
  ///
  ///
  ///Extras
  def checkBoardPos(k:Int):String={
    k match{
      case 1 => "A"
      case 2 => "B"
      case 3 => "C"
      case 4 => "D"
      case 5 => "E"
      case 6 => "F"
      case 7 => "G"
      case 8 => "H"
      case _ => "ll"
    }
  }
  def createChoiceString(listOfStuff:List[String],firstText:String):String={
    var theString = firstText + " ["
    listOfStuff.foreach(value => theString += (value+ ","))
    theString = theString.substring(0,theString.length-1)
    theString += "]"
    return theString
  }

}




///CLASSES
abstract class PieceInformation{
  var theType:String
  var possibleMoves:List[String] = List("FORWARD","LEFT","BACKWARD","RIGHT","FL","FR","BL","BR") //i.e = "L","R","LU","RU")
  var moveDistance:Int = 7 //i.e 1,3,7(max)
  def specialMove()={} //Pawn move 2 for first move, knight move

}
class Rook extends PieceInformation{
  var theType = "Rook"
   possibleMoves = List("FORWARD","LEFT","BACKWARD","RIGHT")
}
class Bishop extends PieceInformation{
  var theType = "Bishop"
  possibleMoves = List("FL","FR","BL","BR")
}
class King extends PieceInformation{
  var theType = "King"
  moveDistance = 1
}
class Queen extends PieceInformation{
  var theType = "Queen"
}
class Pawn extends PieceInformation{
  var theType = "Pawn"
  possibleMoves = List("FORWARD")
  moveDistance = 1
  var firstMove = true
  override def specialMove()= {
    if (firstMove) {
      moveDistance = 2
    }
  }
}
class Knight extends PieceInformation{
  var theType = "knight"
  possibleMoves = List("FORWARD","LEFT","BACKWARD","RIGHT")
  moveDistance = 3
}