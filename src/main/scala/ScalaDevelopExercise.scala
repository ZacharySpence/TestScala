object ScalaDevelopExercise extends App {

  def checkOut()={
    var items = Map("apples"->0.60,"oranges"->0.25)
    var soldItems:List[String] = List()

    def buyItems()={
      print("You can buy ")
      items.keys.foreach(key => print (key+","))
      println("")
      var item = (readLine()+ " ").toLowerCase()
      item.substring(0,1) match{
        case "a" => soldItems ::= "apples"
        case "o" => soldItems ::= "oranges"
      }
    }
    var end = false
    while(!end){
      var choice = (readLine("Do you want to buy something?") + " ").toLowerCase()
      choice.substring(0,1) match{
        case "n" => end = true
        case "y" => buyItems()
        case _ => buyItems()
      }
    }
    var bill = 0.00
    soldItems.foreach(item => bill += items(item))
    println(s"Your basket is $soldItems ")
    println(s"Your bill is £$bill")
  }

  checkOut()
}
