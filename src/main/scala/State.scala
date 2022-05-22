 abstract class State{
  var width:Int
   var height:Int

  private var Size = width*height
  var board = new Array[String](Size)

  def print(): Unit ={
    board.map(x=>println(x))
  }


}
