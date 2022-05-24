class State(private var width:Int, private var height:Int, private var player:Int
                     ,private var action:Boolean){

   def getWidth =width
   def getHeight =height
   def getPlayer =player
   def getAction:Boolean =action
   def setPlayer (value:Int): Unit =
   {
     player=value
   }
   def setAction (value :Boolean): Unit =
  {
    action=value
  }
  private var Size = width*height
  var board = new Array[String](Size)

}
