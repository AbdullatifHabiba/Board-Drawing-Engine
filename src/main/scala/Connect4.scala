class Connect4 {
  val initialBoard: Array[Array[String]] = Array(
    Array("W", "W", "W", "W", "W", "W", "W"),
    Array("W", "W", "W", "W", "W", "W", "W"),
    Array("W", "W", "W", "W", "W", "W", "W"),
    Array("W", "W", "W", "W", "W", "W", "W"),
    Array("W", "W", "W", "W", "W", "W", "W"),
    Array("W", "W", "W", "W", "W", "W", "W")
  )
  var board: Array[Array[String]] = initialBoard
  def Drawer(controller: (State,Input)=>State, state: State,input:Input): Array[Array[String]]  = {
    board = controller.apply(state,input).board;
    //   A  B  C  D   E   F   G
    // 1 .  .  .  .   .   .   .
    // 2 .  .  .  .   .   .   .
    // 3 .  .  .  .   .   .   .
    // 4 .  .  .  .   .   .   .
    // 5 .  .  .  .   .   .   .
    // 6 .  .  .  .   .   .   .
    //2D array.
    print("    A  B  C  D  E  F  G  \n"); //names of the columns on the top
    for( i<- 0 to 5 ) {
      println("")
      print( (i+1)+"   ")
      for(j<- 0 to 6 ) {
        if (board(i)(j)=="i"||board(i)(j)=="j"){print("   ")}
        else {print(board(i)(j)+"  ")}
      }
    }
    println(" ")
    println(" ")
    board
  }
  def Controller(state: State,input:Input): State ={
    if(input.getValue()==null){
      state.board=board
      return state
    }
    val place: String = input.getValue() //A
    val col= place.charAt(0) - 97
    var lastInCol=10
    var player = state.getPlayer
    board=state.board
    if(col<7 && col>=0 ){//we are in the board
      lastInCol= ColumnChecker(col,board)
      println(lastInCol)
      if(lastInCol!=10){//check column not fulled
        if(player==1){//Place X "First player"
          board(lastInCol)(col)="R"
        }
        else {
          board(lastInCol)(col)="Y"
        }
        GameFinished(board)
        state.setAction(true)
        state.board=board
        state
      }
      else { // This column is full
        state.setAction(false)
        state
      }
    }
    else { // insert wrong index
      state.setAction(false)
      state
    }
  }
  def GameFinished(board: Array[Array[String]]): Unit ={
    var finished = true
    for( i<- 0 to 5 ) {
      for(j<- 0 to 6 ) {
        if(board(i)(j)==" "){
          finished=false
        }
      }
    }
    if(finished){
      println("The game is finished.")
    }
  }
  def ColumnChecker(col:Int,board:Array[Array[String]]  ): Int ={
    var i=5
    while( i>= 0 ) {
      println(i)
      if(board(i)(col)=="W"){
        return i
      }
      i-=1
    }
    return 10
  }
}
