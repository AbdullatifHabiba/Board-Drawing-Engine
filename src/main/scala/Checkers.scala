class Checkers {
  val EmptyBoard: Array[Array[String]] = Array(
    Array("W", "B", "W", "B", "W", "B", "W", "B"),
    Array("B", "W", "B", "W", "B", "W", "B", "W"),
    Array("W", "B", "W", "B", "W", "B", "W", "B"),
    Array("B", "W", "B", "W", "B", "W", "B", "W"),
    Array("W", "B", "W", "B", "W", "B", "W", "B"),
    Array("B", "W", "B", "W", "B", "W", "B", "W"),
    Array("W", "B", "W", "B", "W", "B", "W", "B"),
    Array("B", "W", "B", "W", "B", "W", "B", "W")
  )

  val initialBoard: Array[Array[String]] = Array(
    Array(" ", "x", " ", "x", " ", "x", " ", "x"),
    Array("x", " ", "x", " ", "x", " ", "x", " "),
    Array(" ", "x", " ", "x", " ", "x", " ", "x"),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array("o", " ", "o", " ", "o", " ", "o", " "),
    Array(" ", "o", " ", "o", " ", "o", " ", "o"),
    Array("o", " ", "o", " ", "o", " ", "o", " ")
  )

  var board: Array[Array[String]] = Array.ofDim[String](8, 8)

  def Drawer(controller: (State,Input)=>State, state: State,input:Input): Array[Array[String]]  = {
    board = controller.apply(state,input).board;
    var i = 0 to 7
    i.foreach(i => print(board(0)(i)))
    println("")
    i.foreach(i => print(board(1)(i)))
    println("")
    i.foreach(i => print(board(2)(i)))
    println("")
    i.foreach(i => print(board(3)(i)))
    println("")
    i.foreach(i => print(board(4)(i)))
    println("")
    i.foreach(i => print(board(5)(i)))
    println("")
    i.foreach(i => print(board(6)(i)))
    println("")
    i.foreach(i => print(board(7)(i)))
    println("")
    board
  }

  def Controller(state: State, input: Input): State = {
    if (input.getValue() == null) {
      state.board = initialBoard
      return state
    }
    var move: Array[String] = input.getValue().split(",")
    println(s"length of move ${move.length}")
    var player = state.getPlayer
    board = state.board
    if (player == 0) {
      if (!isWhite(move(0)) || move.length > 4) {
        state.setAction(false)
        return state
      }
      var i = 0;
      do {
        if (!moveWhite(move(i), move(i + 1))) {
          state.setAction(false)
          return state
        }
        i += 1
      } while (i < move.length - 1)
    } else {
      if (!isBlack(move(0)) || move.length > 4) {
        state.setAction(false)
        return state
      }
      var i = 0;
      do {
        if (!moveBlack(move(i), move(i + 1))) {
          state.setAction(false)
          return state
        }
        i += 1
      } while (i < move.length - 1)
    }
    state.board = this.board
    state
  }

  def getPosition(move: String): (Int, Int) = {
    var y: Int = 0
    if (move.charAt(0).toInt >= 1 &&
      move.charAt(0).toInt <= 8 &&
      move.charAt(1).compareTo('A') >= 0 &&
      move.charAt(1).compareTo('H') <= 0
    )
      return (-1, -1)
    move.charAt(1) match {
      case 'A' => y = 0
      case 'B' => y = 1
      case 'C' => y = 2
      case 'D' => y = 3
      case 'E' => y = 4
      case 'F' => y = 5
      case 'G' => y = 6
      case 'H' => y = 7
    }
    var x = move.substring(0, 1).toInt - 1
    (x, y)
  }

  def isWhite(move: String): Boolean = {
    var (x, y) = getPosition(move)
    if (x == -1) return false
    board(x)(y) match {
      case "O" => true
      case "o" => true
      case _ => false
    }
  }

  def isBlack(move: String): Boolean = {
    var (x, y) = getPosition(move)
    if (x == -1) return false
    board(x)(y) match {
      case "X" => true
      case "x" => true
      case _ => false
    }
  }

  def isPromomted(move: String): Boolean = {
    var (x, y) = getPosition(move)
    if (x == -1) return false
    board(x)(y) match {
      case "X" => true
      case "O" => true
      case _ => false
    }
  }

  def moveWhite(first: String, second: String): Boolean = {
    var (x1, y1) = getPosition(first)
    var (x2, y2) = getPosition(second)
    if ((x2 > x1 && !isPromomted(first)) ||
      !(math.abs(x2 - x1) == math.abs(y2 - y1)) ||
      isWhite(second) ||
      isBlack(second) ||
      math.abs(x2 - x1) > 2 ||
      x2 == -1 ||
      x1 == -1
    ) {
      return false
    }
    if (math.abs(x2 - x1) == 1) {
      board(x2)(y2) = "o"
      board(x1)(y1) = " "
    } else {
      if (board((x1 + x2) / 2)((y1 + y2) / 2).equalsIgnoreCase("x") ||
        (board((x1 + x2) / 2)((y1 + y2) / 2).equalsIgnoreCase("X") && isPromomted(first))) {
        board((x1 + x2) / 2)((y1 + y2) / 2) = " "
        board(x2)(y2) = "o"
        board(x1)(y1) = " "
      }
    }
    if (x2 == 0) board(x2)(y2) = "O"
    true
  }

  def moveBlack(first: String, second: String): Boolean = {
    var (x1, y1) = getPosition(first)
    var (x2, y2) = getPosition(second)
    if (x2 < x1 && !isPromomted(first) ||
      !(math.abs(x2 - x1) == math.abs(y2 - y1)) ||
      isWhite(second) ||
      isBlack(second) ||
      math.abs(x2 - x1) > 2 ||
      x2 == -1 ||
      x1 == -1
    ) {
      return false
    }
    if (math.abs(x2 - x1) == 1) {
      board(x2)(y2) = "x"
      board(x1)(y1) = " "
    } else {
      if (board((x1 + x2) / 2)((y1 + y2) / 2).equalsIgnoreCase("o") ||
        (board((x1 + x2) / 2)((y1 + y2) / 2).equalsIgnoreCase("O") && isPromomted(first))) {
        board((x1 + x2) / 2)((y1 + y2) / 2) = " "
        board(x2)(y2) = "x"
        board(x1)(y1) = " "
      }
    }
    if (x2 == 7) board(x2)(y2) = "X"
    true
  }
}