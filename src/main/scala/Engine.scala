import play.stage
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
import scalafx.scene.paint.Color.{Brown, LightGray}
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.GridPane.{getColumnIndex, getRowIndex}
import scalafx.scene.layout.Priority
import scalafx.scene.shape.Rectangle

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


 var flag:Boolean=true
  var chessObj:chess=null;
  var checkerObj:Checkers=null;
  var connectObj:Connect4=null;
  var tictoc:TicTacToc=null;

  var stateObj:State=null;
  def engine(obj :Any): Array[Array[String]] = {

    var a = obj
    var arr:Array[Array[String]]=null

    a match {
      case 1 =>{
        if(flag) {
           chessObj = new chess()
           stateObj = new State(8, 8, 1, true)
           arr = chessObj.Drawer(chessObj.Controller(stateObj, new Input(null)))
          flag = false
        }else {
          var inputMove: String = play.getInput()
          print(inputMove)
          var controlObj = chessObj.Controller(stateObj, new Input(inputMove))
          arr = chessObj.Drawer(controlObj)
          if (controlObj.getAction) controlObj.setPlayer({
            if (controlObj.getPlayer == 1) 2 else 1
          })
          stateObj = controlObj
        }
          arr
      }
      case 2 =>{
        var stateObj =new State(7,6,0,true)
        var connect4Obj =new Connect4()
      arr=  connect4Obj.Drawer(connect4Obj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj= connect4Obj.Controller(stateObj,new Input(inputMove))
         arr= connect4Obj.Drawer(controlObj)
          if(controlObj.getAction)controlObj.setPlayer({if(controlObj.getPlayer==1)2 else 1})

          stateObj=controlObj

        }
        arr
      }
      case 3 =>{
        if(flag) {
          checkerObj = new Checkers()
          stateObj = new State(8, 8, 1, true)
          arr = checkerObj.Drawer(checkerObj.Controller(stateObj, new Input(null)))
          flag = false
        }else {
          var inputMove: String = play.getInput()
          print(inputMove+"\n")
          var controlObj = checkerObj.Controller(stateObj, new Input(inputMove))
          arr = checkerObj.Drawer(controlObj)
          if (controlObj.getAction) controlObj.setPlayer({
            if (controlObj.getPlayer == 1) 2 else 1
          })
          stateObj = controlObj
        }
        arr
      }
      case 4 =>{
        var stateObj =new State(3,3,0,true)
        var TicTacObj =new TicTacToc()
       arr= TicTacObj.Drawer(TicTacObj.Controller(stateObj,null))
        while(true){
          var inputMove:String =scala.io.StdIn.readLine("Enter movement: ")
          var controlObj=TicTacObj.Controller(stateObj,new Input(inputMove))
          arr=TicTacObj.Drawer(controlObj)
          if(controlObj.getAction)controlObj.setPlayer({if(controlObj.getPlayer==1)2 else 1})
          stateObj=controlObj

        }
        arr
      }
    }


  }
}



object play extends JFXApp3 {
 var movement:String=null
  override def start(): Unit = {
    var engine=new Engine()

      stage=new JFXApp3.PrimaryStage{
        title= "Engine games"

      }
    val scene1=new Scene(500,500) {

      fill = Color.Blue


      val chess = new Button("Chess") {
        style = "-fx-background-color: gold"
        layoutX = 170
        layoutY = 40

      }
      chess.setPrefSize(200, 50)


      fill = Color.Gray


      val checkers = new Button("Checker") {
        layoutX = 170
        layoutY = 100
        style = "-fx-background-color: gold"

      }
      checkers.onMouseClicked = { (e) => {
        //new Engine().engine(3)
        checkers.style = "-fx-background-color: blue"

        content.foreach(_.disable = true)
        quiet.disable = false


      }
      }
      checkers.setPrefSize(200, 50)
      val connect = new Button("Connect4") {
        layoutX = 170
        layoutY = 160
        style = "-fx-background-color: gold"

      }
      connect.setPrefSize(200, 50)
      connect.onMouseClicked = { (e) => {
        //new Engine().engine(2)
        connect.style = "-fx-background-color: blue"

        content.foreach(_.disable = true)
        quiet.disable = false


      }
      }
      val TicTac = new Button("TicTacToc") {
        layoutX = 170
        layoutY = 220
        style = "-fx-background-color: gold"

      }
      TicTac.setPrefSize(200, 50)
      TicTac.onMouseClicked = { (e) => {
        //new Engine().engine(4)
        TicTac.style = "-fx-background-color: blue"

        content.foreach(_.disable = true)
        quiet.disable = false


      }
      }
      val Reset = new Button("Reset") {
        layoutX = 210
        layoutY = 280
        style = "-fx-background-color: green"

      }
      Reset.setPrefSize(100, 50)

      val quiet = new Button("Quiet") {
        layoutX = 210
        layoutY = 340
        style = "-fx-background-color: red"

      }
      quiet.setPrefSize(100, 50)
      quiet.style = "-fx-background-color: red"
      quiet.onMouseClicked = { (e) => {
        //new Engine().engine(2)
        quiet.style = "-fx-background-color: blue"

        content.foreach(p => {
          p.disable = false
          p.style = "-fx-background-color: gold"
        })
        quiet.style = "-fx-background-color: red"
        Reset.style = "-fx-background-color: green"


      }
      }


      content = List(chess, checkers, connect, TicTac, Reset, quiet)
      val label = new Label("Input")
      label.layoutX = 40
      label.layoutY = 450
      val input = new TextField()
      input.layoutX = 80
      input.layoutY = 450
      input.setMinWidth(10)
      input.setMinHeight(10)
      val ok = new Button("Apply")
      ok.layoutX = 230
      ok.layoutY = 450

      val gridPane = new GridPane();
      var arr: Array[Array[String]] = null
      var count = 0
      gridPane.layoutX = 50
      gridPane.layoutY = 30
      chess.onAction = { (e) => {
        chess.style = "-fx-background-color: blue"
        arr = engine.engine(1)
        val s = 100 // side of rectangle
        for (i <- 0 until 8) {
          count += 1
          for (j <- 0 until 8) {
            val r = new Button()
            r.text = arr(7 - i)(7 - j)
            r.style = "-fx-font: 30 arial;"
            r.setPrefSize(50, 50)
            if (count % 2 == 0) r.style = "-fx-background-color: black" else r.style = "-fx-background-color: white"
            gridPane.add(r, j, i)


            count += 1
          }
        }
        content = List(gridPane, label, input, ok)

      }

        ok.onMouseClicked = { (e) => {
          setInput(input.text.value)

          arr = engine.engine(1)
          print(arr.array)
          for (i <- 0 until 8) {
            count += 1
            for (j <- 0 until 8) {
              val r = new Button()
              r.text = arr(7 - i)(7 - j)
              r.setPrefSize(50, 50)
              if (count % 2 == 0) r.style = "-fx-background-color: black" else r.style = "-fx-background-color: white"
              gridPane.add(r, j, i)
              gridPane.layoutX = 50
              gridPane.layoutY = 30
              count += 1
            }
          }

          content = List(gridPane, label, input, ok)
          // input.text = ""
        }
        }
      }
      checkers.onAction = { (e) => {
        checkers.style = "-fx-background-color: blue"

        arr = engine.engine(3)
        val s = 100
        for (i <- 0 until 8) {
          count += 1
          for (j <- 0 until 8) {
            val r = new Button()
            r.text = arr(i)(j)
            r.style = "-fx-font: 30 arial;"
            r.setPrefSize(50, 50)
            if (count % 2 == 0) r.style = "-fx-background-color: black" else r.style = "-fx-background-color: white"
            gridPane.add(r, j, i)


            count += 1
          }
        }
        content = List(gridPane, label, input, ok)

      }

        ok.onMouseClicked = { (e) => {
          setInput(input.text.value)

          arr = engine.engine(3)
          print(arr.array)
          for (i <- 0 until 8) {
            count += 1
            for (j <- 0 until 8) {
              val r = new Button()
              r.text = arr(i)(j)
              r.setPrefSize(50, 50)
              if (count % 2 == 0) r.style = "-fx-background-color: black" else r.style = "-fx-background-color: white"
              gridPane.add(r, j, i)
              gridPane.layoutX = 50
              gridPane.layoutY = 30
              count += 1
            }
          }

          content = List(gridPane, label, input, ok)
          // input.text = ""
        }
        }
      }
    }
    stage.scene = scene1
    }

  def setInput(in:String)=
    {
    movement=in
    }
  def getInput():String=
  {
   movement
  }

  }

