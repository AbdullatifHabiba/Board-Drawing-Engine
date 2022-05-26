class TicTacToc {
  val EmptyBoard: Array[Array[String]] = Array( //Background
    Array("B", "B", "B"),
    Array("B", "B", "B"),
    Array("B", "B", "B")
  )
  val initialBoard: Array[Array[String]] = Array(
    Array(" ", " ", " "),
    Array(" ", " ", " "),
    Array(" ", " ", " ")
  )
  var board: Array[Array[String]] = initialBoard
  def Drawer(controller: (State,Input)=>State, state: State,input:Input): Array[Array[String]]  = {
    board = controller.apply(state,input).board;
    //   A  B  C
    // 1 X  O  .
    // 2 .  .  .
    // 3 .  .  X
     //2D array.
    print("     A  B  C  \n"); //names of the columns on the top
    for( i<- 0 to 2 ) {
      println("")
      print( (i+1)+"   ")
      for(j<- 0 to 2 ) {
        if (board(i)(j)=="i"|board(i)(j)=="j"){print("   ")}
        else {print(board(i)(j)+"  ")}
      }
    }
    println(" ")
    println(" ")
    board
  }
  def Controller(state: State,input:Input ): State ={
    //return,Action "true or false", Board after movement
    if(input.getValue()==null){
      state.board=board
      return state
    }
    val place: String = input.getValue() //1A //null input
    val row= place.charAt(0) - 49
    val col= place.charAt(1) - 97
    var player = state.getPlayer
    board=state.board
    if((row<3 && row>=0)&& (col<3 && col>=0) ){//we are in the board
      if(board(row)(col)==" "){
        if(player==1){//Place X "First player"
          board(row)(col)="X"
        }
        else {
          board(row)(col)="O"
        }
        GameFinished(board)
        state.setAction(true)
        state.board=board
        state
      }
      else { // there is object already exist
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
    for( i<- 0 to 2 ) {
      for(j<- 0 to 2 ) {
        if(board(i)(j)==" "){
          finished=false
        }
      }
    }
    if(finished){
      println("The game is finished.")
    }
  }
}
