import scalafx.application.JFXApp3
import scalafx.beans.property.{IntegerProperty, ObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.{Button, Label, TableColumn, TableView, TextField}
import scalafx.event.{EventHandler, EventType}
import scalafx.scene.input.KeyEvent

import scala.math.abs
import scala.util.control.Breaks.{break, breakable};
class chess{
  var chess_board: Array[Array[String]] = Array(
    Array("R", "N", "B", "Q", "K", "B", "N", "R"),
    Array("P", "P", "P", "P", "P", "P", "P", "P"),
    Array("j", "i", "j", "i", "j", "i", "j", "i"),
    Array("i", "j", "i", "j", "i", "j", "i", "j"),
    Array("j", "i", "j", "i", "j", "i", "j", "i"),
    Array("j", "j", "i", "j", "i", "j", "i", "j"),
    Array("p", "p", "p", "p", "p", "p", "p", "p"),
    Array("r", "n", "b", "q", "k", "b", "n", "r"))
  val temp_board : Array[Array[String]]= Array(
    Array("j", "i", "j", "i", "j", "i", "j", "i"),
    Array("i", "j", "i", "j", "i", "j", "i","j")
  )
  var chess_printer:Array[Array[String]] =  Array.ofDim[String](8,8)
  def printer (): Unit ={
    for (i<-0 to 7) {
      for (j <- 0 to 7) {
        chess_board(i)(j) match {
          case "R" => chess_printer(i)(j) = Character.toString(9814)
          case "N" => chess_printer(i)(j)  = Character.toString(9816)
          case "B" => chess_printer(i)(j)  = Character.toString(9815)
          case "Q" => chess_printer(i)(j)  = Character.toString(9813)
          case "K" => chess_printer(i)(j)  = Character.toString(9812)
          case "P" => chess_printer(i)(j)  = Character.toString(9817)
          case "r" => chess_printer(i)(j)  = Character.toString(9820)
          case "n" => chess_printer(i)(j)  = Character.toString(9822)
          case "b" => chess_printer(i)(j)  = Character.toString(9821)
          case "q" => chess_printer(i)(j)  = Character.toString(9819)
          case "k" => chess_printer(i)(j)  = Character.toString(9818)
          case "p" => chess_printer(i)(j)  = Character.toString(9823)
          case   _ => chess_printer(i)(j)  = "  "
        }
      }
    }
  }
  var w_attack_piece_rank = 0; var w_attack_piece_file = 0
  var b_attack_piece_rank = 0; var b_attack_piece_file = 0
  var W_rank = 0; var W_file = 4; var B_rank = 7; var B_file = 4
  def Drawer(controller: State ): Unit ={

    /*print("    A   B  C   D  E   F  G   H "); //names of the columns on the top
    for( i<- 0 to 7 ) {
      println("")
      print( i+"   ")
      for(j<- 0 to 7 ) {
        print(chess_printer(i)(j)+"  ")
      }
    }
    println("\n")*/
    ChessGui.start()
  }


  //////////////////////////////////// Control Function ///////////////////////////////////
  def Controller(state: State,input:Input): State ={
    printer()
    if (input.getValue()==null) {
      state.board = chess_board
      state
    }
    else {
      val Input: Array[String] = input.getValue().split("")
      var action: Boolean = true
      val rank_1:Int = Input(0).toInt
      val rank_2:Int = Input(2).toInt
      var file_1:Int = 0
      var file_2:Int = 0
      Input(1).toCharArray.head match {
        case 'A'=> file_1 = 0
        case 'B'=> file_1 = 1
        case 'C'=> file_1 = 2
        case 'D'=> file_1 = 3
        case 'E'=> file_1 = 4
        case 'F'=> file_1 = 5
        case 'G'=> file_1 = 6
        case 'H'=> file_1 = 7
        case  _ => action=false
      }
      Input(3).toCharArray.head match {
        case 'A'=> file_2 = 0
        case 'B'=> file_2 = 1
        case 'C'=> file_2 = 2
        case 'D'=> file_2 = 3
        case 'E'=> file_2 = 4
        case 'F'=> file_2 = 5
        case 'G'=> file_2 = 6
        case 'H'=> file_2 = 7
        case  _ => action=false
      }
      val i = chess_board(rank_1)(file_1).toCharArray.head
      if (state.getPlayer== 1 && i.isUpper | state.getPlayer== 2 && !i.isUpper) {
        if (!(rank_1 < 0 | rank_1 > 7 | file_1 < 0 | file_1 > 7) && action) {
          var temp_W_rank = W_rank ; var temp_W_file = W_file
          var temp_B_rank = B_rank ; var temp_B_file = B_file
          action = valid_move(rank_1, file_1, rank_2, file_2)
          if (action) {
            if (!check_white(W_rank,W_file) && !check_BLACK(B_rank,B_file)) {
              chess_board.apply(rank_2).update(file_2, chess_board(rank_1)(file_1))
              chess_board.apply(rank_1).update(file_1, temp_board(rank_1 % 2)(file_1))
              state.board = chess_board
              state.setPlayer(state.getPlayer)
            }
            else {
              println("Your piece is still under attack")
              W_rank = temp_W_rank ; W_file = temp_W_file
              B_rank = temp_B_rank ; B_file = temp_B_file
            }

          }
          else println("wrong move")
        }
        else println("invalid move ")
      }
      else println("invalid Piece ")
      if (action) {
        if (state.getPlayer == 1 && check_white(W_rank, W_file)) {
          println("Your piece is under attack WHITE player\n")
        }
        if (state.getPlayer == 2 && check_BLACK(B_rank, B_file)) {
          println("Your piece is under attack BLACK Player\n")
        }
      }
      state.setAction(action)
      state
    }
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def valid_move( rank_1 :Int , file_1 :Int ,rank_2 : Int , file_2: Int):Boolean={

    val i = chess_board(rank_1)(file_1).toCharArray.head
    i match {
      case 'R'|'r' =>   rook(rank_1,file_1,rank_2,file_2)
      case 'B'|'b' =>   bishop(rank_1,file_1,rank_2,file_2)
      case 'N'|'n' =>   knight(rank_1,file_1,rank_2,file_2)
      case 'Q'|'q' =>   queen(rank_1,file_1,rank_2,file_2)
      case 'K'|'k' =>   if(king( rank_1, file_1, rank_2, file_2)) {
        if (chess_board(rank_1)(file_1).toCharArray.head == 'K') { W_rank = rank_2; W_file = file_2 }
        else { B_rank = rank_2; B_file = file_2} }
        king( rank_1, file_1, rank_2, file_2)
      case 'P'|'p' =>   pawn(rank_1,file_1,rank_2,file_2)
      case       _ =>   false
    }
  }
  //////////// KING MOVEMENT FUNCTION   ///////////

  def king ( rank_1 : Int ,file_1 : Int ,rank_2 : Int , file_2: Int): Boolean ={
    val i = chess_board(rank_2)(file_2).toCharArray.head
    val x = abs(rank_1 - rank_2)
    val y = abs(file_1 - file_2)
    //the king can move only one step in any direction
    if(x == 1 || y == 1) {
      if(chess_board(rank_2)(file_2) == "i" | chess_board(rank_2)(file_2) == "j") {
        true
      }
      else {
        chess_board(rank_1)(file_1) match {
          case "K" => if (!i.isUpper)  true  else  false
          case "k" => if (i.isUpper)  true  else  false
          case _ =>  false
        }
      }
    }
    else  false
  }

  ///////////// ROOK MOVEMENT FUNCTION ////////////

  def rook ( rank_1 : Int ,file_1 : Int ,rank_2 : Int , file_2: Int):Boolean= {
    var valid = 0
    val i = chess_board(rank_2)(file_2).toCharArray.head
    if (rank_1 == rank_2 && file_1 != file_2) {
      val n = abs(file_2 - file_1)
      //check for one step
      if (n == 1) {
        valid = 1
      }
      /*check that there is nothing to prevent the move*/
      else {
        breakable {
          for (z <- 1 until n) {
            //moving right
            if (file_1 < file_2) {
              if (chess_board(rank_1)(file_1 + z) != "i" && chess_board(rank_1)(file_1 + z) != "j") {
                valid = 0
                break
              }
              else {
                valid = 1
              }
            }
            //moving left
            else {
              if (chess_board(rank_1)(file_1 - z) != "i" && chess_board(rank_1)(file_1 - z) != "j") {
                valid = 0
                break
              }
              else {
                valid = 1
              }
            }
          }
        }
      }
    }
    //move in ranks
    else if (rank_1 != rank_2 && file_1 == file_2) {
      val m = abs(rank_1 - rank_2)
      if (m == 1)
        valid= 1
      else {
        breakable {
          for (w <- 1 until m) {
            //move upwards
            if (rank_1 > rank_2) {
              if (chess_board(rank_1 - w)(file_1) != "i" && chess_board(rank_1 - w)(file_1) != "j") {
                valid = 0
                break
              }
              else {
                valid = 1
              }
            }
            //move downwards
            else {
              if (chess_board(rank_1 + w)(file_1) != "i" && chess_board(rank_1 + w)(file_1) != "j") {
                valid = 0
                break
              }
              else {
                valid = 1
              }
            }
          }
        }
      }
    }
    else {
      valid= 0
    }
    valid match {
      case 1 =>
        if (chess_board(rank_2)(file_2) == "i" | chess_board(rank_2)(file_2) == "j")  true
        else {
          chess_board(rank_1)(file_1) match {
            case "R" => if (!i.isUpper)  true else  false
            case "r" => if (i.isUpper)  true else  false
            case _ =>  true
          }
        }
      case 0 =>  false
    }
  }

  //////////// QUEEN MOVEMENT FUNCTION  ///////////

  def queen ( rank_1 : Int ,file_1 : Int ,rank_2 : Int , file_2: Int):Boolean={
    val r = rook(rank_1,file_1,rank_2,file_2)
    val b = bishop(rank_1,file_1,rank_2,file_2)
    val i = chess_board(rank_2)(file_2).toCharArray.head

    if (r | b){
      if (chess_board(rank_2)(file_2) == "i" | chess_board(rank_2)(file_2) == "j")  true
      else {
        chess_board(rank_1)(file_1) match {
          case "Q" => if (!i.isUpper)  true else  false
          case "q" => if (i.isUpper)  true else  false
          case _ =>  false
        }
      }
    }
    else  false
  }

  //////////// PAWN MOVEMENT FUNCTION  ///////////

  def pawn ( rank_1 : Int ,file_1 : Int ,rank_2 : Int , file_2: Int):Boolean={
    val x = abs(rank_1 - rank_2)
    //white pawn movement
    if(x == 1 && file_1 == file_2 && rank_1 > rank_2 && chess_board(rank_1)(file_1) == "p" &&
      (chess_board(rank_2)(file_2) == "i" || chess_board(rank_2)(file_2) == "j"))
      true
    //black pawn movement
    else if(x == 1 && file_1 == file_2 && rank_1 < rank_2 && chess_board(rank_1)(file_1) == "P" &&
      (chess_board(rank_2)(file_2) == "i" || chess_board(rank_2)(file_2) == "j"))
      true
    //first two steps for the pawns
    else if(x == 2 && file_1 == file_2)
    {
      if((chess_board(rank_1)(file_1) == "p" && rank_1 == 6 && rank_1 > rank_2) ||
        (chess_board(rank_1)(file_1) == "P" && rank_1 == 1 && rank_1 < rank_2))
        true
      else { false }
    }
    //move to attack
    else if(chess_board(rank_1)(file_1) == "P" && rank_2 == rank_1 + 1 && file_2 == file_1 + 1 &&
      chess_board(rank_2)(file_2) != "i" && chess_board(rank_2)(file_2) != "j")
      true
    else if(chess_board(rank_1)(file_1) == "P" &&rank_2 == rank_1 + 1 && file_2 == file_1 - 1 &&
      chess_board(rank_2)(file_2) != "i" && chess_board(rank_2)(file_2) != "j")
      true
    else if(chess_board(rank_1)(file_1) == "p" &&rank_2 == rank_1 - 1 && file_2 == file_1 + 1 &&
      chess_board(rank_2)(file_2) != "i" && chess_board(rank_2)(file_2) != "j")
      true
    else if(chess_board(rank_1)(file_1) == "p" &&rank_2 == rank_1 - 1 && file_2 == file_1 - 1 &&
      chess_board(rank_2)(file_2) != "i" && chess_board(rank_2)(file_2) != "j")
      true
    else  false

  }

  //////////// KNIGHT MOVEMENT FUNCTION ///////////

  def knight ( rank_1 : Int ,file_1 : Int ,rank_2 : Int , file_2: Int):Boolean={
    //the knight move in shape of L 2 steps then 1
    val i = chess_board(rank_2)(file_2).toCharArray.head
    var valid = 0

    if (rank_1 == rank_2 + 2)
    {
      if (file_1 == file_2 + 1)
        valid = 1
      else if (file_1 == file_2 - 1)
        valid = 1
      else
        valid = 0
    }
    else if (rank_1 == rank_2 - 2)
    {
      if (file_1 == file_2 + 1)
        valid = 1
      else if (file_1 == file_2 - 1)
        valid = 1
      else
        valid = 0
    }
    else if (file_1 == file_2 + 2)
    {
      if (rank_1 == rank_2 + 1)
        valid = 1
      else if (rank_1 == rank_2 - 1)
        valid = 1
      else
        valid = 0
    }
    else if (file_1 == file_2 - 2)
    {
      if (rank_1 == rank_2 + 1)
        valid = 1
      else if (rank_1 == rank_2 - 1)
        valid = 1
      else
        valid = 0
    }
    else  valid = 0

    if(valid==1){
      if (chess_board(rank_2)(file_2) == "i" | chess_board(rank_2)(file_2) == "j") true
      else {
        chess_board(rank_1)(file_1) match {
          case "N" => if (!i.isUpper)  true else  false
          case "n" => if (i.isUpper)  true else  false
          case _ =>  false
        }
      }
    }
    else  false}

  //////////// BISHOP MOVEMENT FUNCTION ///////////

  def bishop ( rank_1 : Int ,file_1 : Int ,rank_2 : Int , file_2: Int):Boolean={

    var valid = 0
    val i = chess_board(rank_2)(file_2).toCharArray.head
    val s = abs(rank_1 - rank_2)
    val v = abs(file_1 - file_2)
    if(v == s)
    {
      //move only one step
      if(s == 1)
      {
        valid = 1
      }
      else
      {
        //check if there was any piece in the way before moving
        for (w <- 1 until s) {
          if (rank_1 > rank_2 && file_1 < file_2) //move eastern north
          {
            if (chess_board(rank_1 - w)(file_1+w) == "i" | chess_board(rank_1 - w)(file_1 + w) == "j") {
              valid = 1
            }
            else {
              return false
            }
          }
          else if (rank_1 > rank_2 && file_1 > file_2) //move western north
          {
            if (chess_board(rank_1 - w)(file_1 - w) == "i" | chess_board(rank_1 - w)(file_1 - w) == "j") {
              valid = 1
            }
            else {
              return false
            }
          }
          else if (rank_1 < rank_2 && file_1 < file_2) //move eastern south
          {
            if (chess_board(rank_1 + w)(file_1 + w) == "i" | chess_board(rank_1 + w)(file_1 + w) == "j") {
              valid = 1
            }
            else {
              return false
            }
          }
          else //move western south
          {
            if (chess_board(rank_1 + w)(file_1 - w) == "i" | chess_board(rank_1 + w)(file_1 - w) == "j") {
              valid = 1
            }
            else {
              return false
            }
          }

        }
      }
    }
    else return false
    if(valid==1){
      if (chess_board(rank_2)(file_2) == "i" | chess_board(rank_2)(file_2) == "j")  true
      else {
        chess_board(rank_1)(file_1) match {
          case "B" => if (!i.isUpper)  true else  false
          case "b" => if (i.isUpper)  true else  false
          case _ =>  true
        }
      }
    }
    else  false
  }
  def check_white( w_k_rank: Int, w_k_file: Int): Boolean = {
    var peace = 1
    breakable {
      for (i <- 0 to 7) {
        for (j <- 0 to 7) {
          chess_board(i)(j) match {
            case "r" =>
              if (rook( i, j, w_k_rank, w_k_file)) {
                w_attack_piece_rank = i
                w_attack_piece_file = j
                peace = 0
                break
              }
              else
                peace = 1
            case "q" =>
              if (queen( i, j, w_k_rank, w_k_file)) {
                w_attack_piece_rank = i
                w_attack_piece_file = j
                peace = 0
                break
              }
              else
                peace = 1;
            case "n" =>
              if (knight( i, j, w_k_rank, w_k_file)) {
                w_attack_piece_rank = i
                w_attack_piece_file = j
                peace = 0
                break
              }
              else
                peace = 1;
            case "b" =>
              if (bishop( i, j, w_k_rank, w_k_file)) {
                w_attack_piece_rank = i
                w_attack_piece_file = j
                peace = 0
                break
              }
              else peace = 1;
            case "p" =>
              if ((i == w_k_rank - 1 && j == w_k_file - 1) || (i == w_k_rank - 1 && j == w_k_file + 1)) {
                w_attack_piece_rank = i;
                w_attack_piece_file = j;
                peace = 0
                break
              }
              else peace = 1;
            case _ =>
              peace = 1;
          }
        }
      }
    }
    if (peace==1) false
    else true
  }

  def check_BLACK( B_k_rank: Int, B_k_file: Int): Boolean = {
    var peace = 0;
    breakable {
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          chess_board(i)(j) match {
            case "R" =>
              if (rook( i, j, B_k_rank, B_k_file)) {
                b_attack_piece_rank = i;
                b_attack_piece_file = j;
                peace = 0
                break
              }
              else
                peace = 1;
            case "Q" =>
              if (queen( i, j, B_k_rank, B_k_file)) {
                b_attack_piece_rank = i;
                b_attack_piece_file = j;
                peace = 0
                break
              }
              else
                peace = 1;
            case "N" =>
              if (knight( i, j, B_k_rank, B_k_file)) {
                b_attack_piece_rank = i;
                b_attack_piece_file = j;
                peace = 0
                break
              }
              else
                peace = 1;
            case "B" =>
              if (bishop(i, j, B_k_rank, B_k_file)) {
                b_attack_piece_rank = i;
                b_attack_piece_file = j;
                peace = 0
                break
              }
              else
                peace = 1;
            case "P" =>
              if ((i == B_k_rank - 1 && j == B_k_file - 1) || (i == B_k_rank - 1 && j == B_k_file + 1)) {
                b_attack_piece_rank = i;
                b_attack_piece_file = j;
                peace = 0
                break
              }
              else
                peace = 1;
            case _ =>
              peace = 1;
          }
        }
      }
    }
    if (peace==1 ) false
    else true
  }

}
object ChessGui extends JFXApp3 {

  val col = ObservableBuffer[piece]()
  val R:String = Character.toString(9814)
  val N:String = Character.toString(9816)
  val B:String = Character.toString(9815)
  val Q:String = Character.toString(9813)
  val K:String = Character.toString(9812)
  val P:String = Character.toString(9817)
  val r:String = Character.toString(9820)
  val n:String = Character.toString(9822)
  val b:String = Character.toString(9821)
  val q:String = Character.toString(9819)
  val k:String = Character.toString(9818)
  val p:String = Character.toString(9823)

  val s:String = " " //space

  class piece(s0:String,s1 : String,s2:String,s3:String,s4:String,s5:String,s6:String,s7:String,s8:String) {
    val a0 = new StringProperty(this, "piece", s0)
    val a1 = new StringProperty(this, "piece", s1)
    val a2 = new StringProperty(this, "piece", s2)
    val a3 = new StringProperty(this, "piece", s3)
    val a4 = new StringProperty(this, "piece", s4)
    val a5 = new StringProperty(this, "piece", s5)
    val a6 = new StringProperty(this, "piece", s6)
    val a7 = new StringProperty(this, "piece", s7)
    val a8 = new StringProperty(this, "piece", s8)
  }
  var Input=new Input(null);

  val chess_board = ObservableBuffer[piece](
    new piece(" ","1", "2", "3", "4", "5", "6", "7", "8"),
    new piece("1", R, N, B, Q, K, B, N, R),
    new piece("2", P, P, P, P, P, P, P, P),
    new piece("3", s, s, s, s, s, s, s, s),
    new piece("4", s, s, s, s, s, s, s, s),
    new piece("5", s, s, s, s, s, s, s, s),
    new piece("6", s, s, s, s, s, s, s, s),
    new piece("7", p, p, p, p, p, p, p, p),
    new piece("8", r, n, b, q, k, b, n, r))
  override def start(): Unit = {

    stage = new JFXApp3.PrimaryStage {
      title = "Simple TableView"
      scene = new Scene(450,240) {
        val app = new TableView[piece](chess_board) {

          columns ++= List(
            new TableColumn[piece, String] {


              cellValueFactory = {

                _.value.a0

              }
              prefWidth = 50
            },
            new TableColumn[piece, String] {

              cellValueFactory = {
                _.value.a1
              }


              prefWidth = 50


            },
            new TableColumn[piece, String] {
              cellValueFactory = {
                _.value.a2
              }
              prefWidth = 50
            },
            new TableColumn[piece, String] {
              cellValueFactory = {
                _.value.a3
              }
              prefWidth = 50

            },
            new TableColumn[piece, String] {
              cellValueFactory = {
                _.value.a4
              }
              prefWidth = 50
            },
            new TableColumn[piece, String] {
              cellValueFactory = {
                _.value.a5
              }
              prefWidth = 50

            },
            new TableColumn[piece, String] {
              cellValueFactory = {
                _.value.a6
              }
              prefWidth = 50
            },
            new TableColumn[piece, String] {
              cellValueFactory = {
                _.value.a7
              }
              prefWidth = 50


            },
            new TableColumn[piece, String] {

              cellValueFactory = {
                _.value.a8
              }
              prefWidth = 50

            }
          )


        }
        val label = new Label("Input")
        label.layoutX = 40
        label.layoutY = app.getHeight
        val input = new TextField()
        input.layoutX = 80
        input.layoutY = app.getHeight
        input.setMinWidth(10)
        input.setMinHeight(10)
        val ok = new Button("Apply")
        ok.layoutX = 230
        ok.layoutY = app.getHeight
        ok.onMouseClicked={(e)=>{Input.settingValue(input.getText())}}
        content = List(app, label,input,ok)
      }

    }
  }
}