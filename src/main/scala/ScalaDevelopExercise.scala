object ScalaDevelopExercise extends App {

  def checkOut()={
    var items = Map("apples"->0.60,"oranges"->0.25)
    var soldItems:List[String] = List()
    var freeItems:List[String] = List()
    var orangeCount = 0

    def buyItems()={
      print("You can buy ")
      items.keys.foreach(key => print (key+","))
      println("")
      var item = (readLine()+ " ").toLowerCase()
      item.substring(0,1) match{
        case "a" =>  soldItems ::= "apples"
          println("Here's your free apple")
          freeItems ::= "apples"
        case "o" => soldItems ::= "oranges"
          orangeCount += 1
          if (orangeCount%2 ==0){
            println("Here's your free orange")
            freeItems ::= "oranges"
          }
      }
    }
    var end = false
    println("The offers today are buy 1 apple get one free AND 3 oranges for the price of 2")
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
    var totalItems = soldItems ::: freeItems
    println(s"Your basket is $totalItems")
    println(s"Your bill is £$bill")
  }

  checkOut()
}
