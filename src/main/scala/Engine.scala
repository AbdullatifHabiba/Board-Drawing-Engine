
class Engine {
/*
choose game
main start get input and build it
* input is generic

**ex:chess
then called engine(chess.drawer,chess.controller)
* هتعمل while true
* مادام اختار اللعبة صح هيكمل و
* بعدان يعمل state للعبة وبعدان يبعتها ل controller
*
* وبعدان يبعتها ل drawer
* engine(Draw(Control))
*
* controller(state,input,player) w drawer functions
player 1 or 2
* */

  def engine(obj :Any): Unit = {
    print( """Choose game :
             |1-chess
             |2-Connect-4
             |3-Checkers
             |4-Tic-Tac-Toe
             |""".stripMargin)
    var a = scala.io.StdIn.readInt()
    a match {
      case 1 =>{
        var stateObj =new State(8,8,0,true)
        var chessObj =new chess()
        chessObj.Drawer(chessObj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj=chessObj.Controller(stateObj,new Input(inputMove))
          chessObj.Drawer(controlObj) // if state is correct you should increment playernumb
        }
      }
      case 2 =>{
        var stateObj =new State(7,6,0,true)
        var connect4Obj =new Connect4()
        connect4Obj.Drawer(connect4Obj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj= connect4Obj.Controller(stateObj,new Input(inputMove))
          connect4Obj.Drawer(controlObj) // if state is correct you should increment playernumb
        }
      }
      case 3 =>{
        var stateObj =new State(8,8,0,true)
        var CheckersObj =new Checkers()
        CheckersObj.Drawer(CheckersObj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj=CheckersObj.Controller(stateObj,new Input(inputMove))
          CheckersObj.Drawer(controlObj) // if state is correct you should increment playernumb
        }
      }
      case 4 =>{
        var stateObj =new State(3,3,0,true)
        var TicTacObj =new TicTacToc()
        TicTacObj.Drawer(TicTacObj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj=TicTacObj.Controller(stateObj,new Input(inputMove))
          TicTacObj.Drawer(controlObj) // if state is correct you should increment playernumb
        }
      }
    }


  }
}
object main{
  def main(args: Array[String]): Unit = {

   var engine=new Engine()
   engine.engine()
  }
  }
