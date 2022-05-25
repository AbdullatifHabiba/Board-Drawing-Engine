import scalafx.scene.control.{Button, Label, TextArea, TextField}
import scalafx.Includes._
import scalafx.Includes.when
import scalafx.scene.Scene
import scalafx.application.JFXApp3
import scalafx.beans.property.StringProperty
import scalafx.event.ActionEvent
import scalafx.scene.effect.BlendMode.{Blue, Green, Red}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{Background, BackgroundFill, CornerRadii}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.Brown

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
    var a = obj
    a match {
      case 1 =>{
        val T =new Thread()
          T.start();
        var stateObj =new State(8,8,1,true)
        var chessObj =new chess()
        chessObj.Drawer(chessObj.Controller(stateObj,new Input(null)))
        while(ChessGui.stage.showing.value){
          var inputMove:String = ChessGui.Input.getValue()
          if(inputMove!=null)T.notify()
          var controlObj=chessObj.Controller(stateObj,new Input(inputMove))
          chessObj.Drawer(controlObj)
          if(controlObj.getAction)controlObj.setPlayer({if(controlObj.getPlayer==1)2 else 1})
          stateObj=controlObj
        }
        T.wait()

      }
      case 2 =>{
        var stateObj =new State(7,6,0,true)
        var connect4Obj =new Connect4()
        connect4Obj.Drawer(connect4Obj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj= connect4Obj.Controller(stateObj,new Input(inputMove))
          connect4Obj.Drawer(controlObj)
          if(controlObj.getAction)controlObj.setPlayer({if(controlObj.getPlayer==1)2 else 1})

          stateObj=controlObj

        }
      }
      case 3 =>{
       var stateObj =new State(8,8,0,true)
        var CheckersObj =new Checkers()
        CheckersObj.Drawer(CheckersObj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj=CheckersObj.Controller(stateObj,new Input(inputMove))
          CheckersObj.Drawer(controlObj)
          if(controlObj.getAction)controlObj.setPlayer({if(controlObj.getPlayer==1)2 else 1})

          stateObj=controlObj

        }
      }
      case 4 =>{
        var stateObj =new State(3,3,0,true)
        var TicTacObj =new TicTacToc()
        TicTacObj.Drawer(TicTacObj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj=TicTacObj.Controller(stateObj,new Input(inputMove))
          TicTacObj.Drawer(controlObj)
          if(controlObj.getAction)controlObj.setPlayer({if(controlObj.getPlayer==1)2 else 1})
          stateObj=controlObj

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

object play extends JFXApp3 {

  override def start(): Unit = {
    var engine=new Engine()

      stage=new JFXApp3.PrimaryStage{
        title= "Engine games"
         scene=new Scene(500,500) {

           fill = Brown

           val chess = new Button("Chess"){
             style = "-fx-background-color: gold"
             layoutX = 170
             layoutY = 40

           }
           chess.onMouseClicked={(e)=>{
            new Engine().engine(1)
             chess.style="-fx-background-color: blue"
             print(chess.getId,chess.getUserData,chess.getText)

             content.foreach(_.disable = true)
             quiet.disable = false

           }}

           chess.setPrefSize(200, 50)

           val checkers = new Button("Checker")
           {
             layoutX = 170
             layoutY = 100
             style = "-fx-background-color: gold"

           }
           checkers.onMouseClicked={(e)=>{
             //new Engine().engine(3)
             checkers.style="-fx-background-color: blue"

             content.foreach(_.disable = true)
             quiet.disable = false


           }}
           checkers.setPrefSize(200, 50)
           val connect = new Button("Connect4") {
             layoutX = 170
             layoutY = 160
             style = "-fx-background-color: gold"

           }
           connect.setPrefSize(200, 50)
           connect.onMouseClicked={(e)=>{
             //new Engine().engine(2)
             connect.style="-fx-background-color: blue"

             content.foreach(_.disable = true)
             quiet.disable = false


           }}
           val TicTac = new Button("TicTacToc") {
             layoutX = 170
             layoutY = 220
             style = "-fx-background-color: gold"

           }
           TicTac.setPrefSize(200, 50)
           TicTac.onMouseClicked={(e)=>{
             //new Engine().engine(4)
             TicTac.style="-fx-background-color: blue"

             content.foreach(_.disable = true)
             quiet.disable = false


           }}
           //TicTac.style = "-fx-background-color: green"
           val Reset = new Button("Reset"){
             layoutX = 210
             layoutY = 280
             style = "-fx-background-color: green"

           }
           Reset.setPrefSize(100, 50)

           val quiet = new Button("Quiet")
           {
           layoutX = 210
           layoutY = 340
             style = "-fx-background-color: red"

           }
           quiet.setPrefSize(100, 50)
           quiet.style = "-fx-background-color: red"
           quiet.onMouseClicked={(e)=>{
             //new Engine().engine(2)
             quiet.style="-fx-background-color: blue"

             content.foreach(p=>{p.disable = false
             p.style = "-fx-background-color: gold"
             })
             quiet.style = "-fx-background-color: red"
             Reset.style = "-fx-background-color: green"



           }}

           content = List(chess, checkers, connect, TicTac, Reset, quiet)

         }

      }
    }



  }

