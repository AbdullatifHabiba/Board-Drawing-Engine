import play.stage
import scalafx.scene.control.{Alert, Button, Label, TextArea, TextField}
import scalafx.Includes._
import scalafx.Includes.when
import scalafx.scene.Scene
import scalafx.application.JFXApp3
import scalafx.beans.property.StringProperty
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.effect.BlendMode.{Blue, Green, Red}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{Background, BackgroundFill, BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundSize, Border, CornerRadii, GridPane, Priority}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.{Brown, Chocolate, Coral, DARKVIOLET, LightGray}
import scalafx.scene.layout.GridPane.{getColumnIndex, getRowIndex}
import scalafx.scene.shape.{Circle, Rectangle}
import scalafx.scene.text.Font
import scalafx.scene.web.WebEvent.Alert

import scala.reflect.internal.util.Collections

class Engine {
  def engine(drawer: ((State, Input) => State, State, Input) => Array[Array[String]], controller: (State, Input) => State, state: State, input: Input): Array[Array[String]] = {
    drawer.apply(controller, state, input)
  }
}


object play extends JFXApp3 {
  var movement: String = null

  override def start(): Unit = {
    var engine = new Engine()

    stage = new JFXApp3.PrimaryStage {
      title = "Engine Games"

    }
    val scene1 = new Scene(500, 500) {
      fill = DARKVIOLET


      val chess = new Button("Chess") {
        style = "-fx-background-color: gold"
        layoutX = 170
        layoutY = 60

      }
      chess.setPrefSize(200, 50)


      val checkers = new Button("Checker") {
        layoutX = 170
        layoutY = 140
        style = "-fx-background-color: gold"

      }


      checkers.setPrefSize(200, 50)
      val connect = new Button("Connect4") {
        layoutX = 170
        layoutY = 220
        style = "-fx-background-color: gold"

      }
      connect.setPrefSize(200, 50)

      val TicTac = new Button("TicTacToc") {
        layoutX = 170
        layoutY = 300
        style = "-fx-background-color: gold"

      }
      TicTac.setPrefSize(200, 50)

      val Reset = new Button("Reset") {
        layoutX = 300
        layoutY = 450

        style = "-fx-background-color: green"

      }
      Reset.setPrefSize(50, 30)

      val quiet = new Button("Quiet") {
        layoutX = 350
        layoutY = 450

        style = "-fx-background-color: red"

      }
      quiet.setPrefSize(50, 30)
      quiet.style = "-fx-background-color: red"
      quiet.onMouseClicked = { (e) => {

        quiet.style = "-fx-background-color: red"
        Reset.style = "-fx-background-color: green"
       stage.title = "Engine Games"
        TicTac.cancelButton=true
        checkers.cancelButton=true
        connect.cancelButton=true
        chess.cancelButton=true

        content = List(chess, checkers, connect, TicTac)
      }
      }


      content = List(chess, checkers, connect, TicTac)
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


      var chessObj: chess = null;
      var stat: State = null;
      var in: Input = null
      chess.onAction = { (e) => {
        chessObj = new chess()
        stat = new State(8, 8, 1, true)
        in = new Input(null)
        if (!stat.getAction) println("error Action")
        arr = engine.engine(chessObj.Drawer, chessObj.Controller, stat, in)
        val s = 100 // side of rectangle
        for (i <- 0 until 8) {
          count += 1
          for (j <- 0 until 8) {
            val r = new Button()
            r.text = printer(arr(7 - i)(7 - j))
            r.setPrefSize(50, 50)
            if (count % 2 == 0) r.style = "-fx-background-color: black;" else r.style = "-fx-background-color: white;"
            gridPane.add(r, j, i)


            count += 1
          }
        }
        content = List(gridPane, label, input, ok, Reset, quiet)

      }

        ok.onMouseClicked = { (e) => {
          stat = chessObj.Controller(stat, in)
          in = new Input(input.text.value)
          if (!chessObj.Controller(stat, in).getAction) println("Error Action")
          else {
            stat.setPlayer({
              if (stat.getPlayer == 1) 2 else 1
            })
            arr = engine.engine(chessObj.Drawer, chessObj.Controller, stat, in)
            for (i <- 0 until 8) {
              count += 1
              for (j <- 0 until 8) {
                val r = new Button()
                r.text = printer(arr(i)(j))
                r.setPrefSize(50, 50)
                if (count % 2 == 0) r.style = "-fx-background-color: black;" else r.style = "-fx-background-color: white;"
                gridPane.add(r, j, i)

                count += 1
              }
            }
          }
          gridPane.layoutX = 50
          gridPane.layoutY = 30
          content = List(gridPane, label, input, ok, Reset, quiet)
        }
        }
      }
      var checker: Checkers = null;
      checkers.onAction = { (e) => {
        checker = new Checkers()
        stat = new State(8, 8, 1, true)
        in = new Input(null)
        if (!stat.getAction) println("error Action")
        arr = engine.engine(checker.Drawer, checker.Controller, stat, in)
        for (i <- 0 until 8) {
          count += 1
          for (j <- 0 until 8) {
            val r = new Button()

            r.setPrefSize(50, 50)
            if (count % 2 == 0) r.style = "-fx-background-color: black;-fx-font: 20 arial;" else r.style = "-fx-background-color: white;-fx-font: 20 arial;"

              /*   if(arr(i)(j)=="x")
                    {
                      r.setStyle(
                        "-fx-background-radius: 20em; " +
                          "-fx-min-width: 40px; " +
                          "-fx-min-height: 40px; " +
                          "-fx-max-width: 40px; " +
                          "-fx-max-height: 40px;" +
                          "-fx-background-color: black;-fx-border-width:5px;-fx-border-color:white;"

                      );
                    }
                  if(arr(i)(j)=="o"){
                    r.setStyle(
                      "-fx-background-radius: 20em; " +
                        "-fx-min-width: 40px; " +
                        "-fx-min-height: 40px; " +
                        "-fx-max-width: 40px; " +
                        "-fx-max-height: 40px;" +
                        "-fx-background-color: white;"

                    );
                 }*/
            r.text = arr(i)(j)
            gridPane.style="-fx-border-width:10px;-fx-border-color:black;-fx-background-color: black;"
            gridPane.add(r, j, i)


            count += 1
          }
        }
        content = List(gridPane, label, input, ok, Reset, quiet)

      }

        ok.onMouseClicked = { (e) => {
          in = new Input(input.text.value)

          if (!checker.Controller(stat, in).getAction) println("Error Action")
          else {

            arr = engine.engine(checker.Drawer, checker.Controller, stat, in)
            stat = checker.Controller(stat, in)

            //(arr.array)
            for (i <- 0 until 8) {
              count += 1
              for (j <- 0 until 8) {
                val r = new Button()
                r.text = arr(i)(j)
                r.setPrefSize(50, 50)
                if (count % 2 == 0) r.style = "-fx-background-color: black;-fx-font: 20 arial;" else r.style = "-fx-background-color: white;-fx-font: 20 arial;"
                gridPane.add(r, j, i)
                gridPane.layoutX = 50
                gridPane.layoutY = 30
                count += 1
              }
            }
            stat.setPlayer({
              if (stat.getPlayer == 1) 2 else 1
            })
          }

          content = List(gridPane, label, input, ok, Reset, quiet)
        }
        }
      }
      Reset.onMouseClicked = { (e) => {

        quiet.style = "-fx-background-color: red"
        Reset.style = "-fx-background-color: green"
        content = List(gridPane, label, input, ok, Reset, quiet)

      }
      }
      var connect4: Connect4 = null;
      connect.onAction = { (e) => {
        connect4 = new Connect4()
        stage.title = "Connect4"

        stat = new State(6, 7, 1, true)
        in = new Input(null)
        if (!stat.getAction) println("error Action")
        arr = engine.engine(connect4.Drawer, connect4.Controller, stat, in)
        for (i <- 0 until 6) {
          count += 1
          for (j <- 0 until 7) {
            val r = new Button()
            r.setStyle(
              "-fx-background-radius: 20em; " +
                "-fx-min-width: 50px; " +
                "-fx-min-height: 50px; " +
                "-fx-max-width: 50px; " +
                "-fx-max-height: 50px;" +
                "-fx-background-color: white;"
            );

            //r.text = arr(i)(j)
            gridPane.add(r, j, i)


            count += 1
          }
        }
        gridPane.layoutX = 60
        gridPane.layoutY = 30
        content = List(gridPane, label, input, ok, Reset, quiet)

      }


        ok.onMouseClicked = { (e) => {
          stat = connect4.Controller(stat, in)
          in = new Input(input.text.value)
          if (!connect4.Controller(stat, in).getAction) println("Error Action")
          else {
            stat.setPlayer({
              if (stat.getPlayer == 1) 2 else 1
            })
            print(stat.getPlayer)
            arr = engine.engine(connect4.Drawer, connect4.Controller, stat, in)
            //(arr.array)
            for (i <- 0 until 6) {
              count += 1
              for (j <- 0 until 7) {
                val r = new Button()
               // r.text = arr(i)(j)
                if (arr(i)(j) == "R") {
                  r.setStyle(
                    "-fx-background-radius: 20em; " +
                      "-fx-min-width: 50px; " +
                      "-fx-min-height: 50px; " +
                      "-fx-max-width: 50px; " +
                      "-fx-max-height: 50px;" +
                      "-fx-background-color: red;"
                  );
                }else if (arr(i)(j) == "Y") {
                  print(arr(i)(j))
                  r.setStyle(
                    "-fx-background-radius: 20em; " +
                      "-fx-min-width: 50px; " +
                      "-fx-min-height: 50px; " +
                      "-fx-max-width: 50px; " +
                      "-fx-max-height: 50px;" +
                      "-fx-background-color: orange;"
                  );
                }
                else {
                  r.setStyle(
                    "-fx-background-radius: 20em; " +
                      "-fx-min-width: 50px; " +
                      "-fx-min-height: 50px; " +
                      "-fx-max-width: 50px; " +
                      "-fx-max-height: 50px;" +
                      "-fx-background-color: white;"
                  );
                }
                gridPane.add(r, j, i)

                count += 1
              }
            }
          }

          content = List(gridPane, label, input, ok, Reset, quiet)
        }
        }
      }
      var XO: TicTacToc = null;
      TicTac.onAction = { (e) => {
        XO = new TicTacToc()
        stage.title = "TicTacTOC"
        gridPane.layoutX = 60
        stat = new State(3, 3, 1, true)
        in = new Input(null)

        if (!stat.getAction) println("error Action")
        arr = engine.engine(XO.Drawer, XO.Controller, stat, in)
        for (i <- 0 until 3) {
          count += 1
          for (j <- 0 until 3) {
            val r = new Button()
            r.setPrefSize(60, 60)
            r.text = arr(i)(j)
            gridPane.add(r, j, i)


            count += 1
          }
        }
        gridPane.layoutX = 150
        gridPane.layoutY = 150
        content = List(gridPane, label, input, ok, Reset, quiet)

      }


        ok.onMouseClicked = { (e) => {
          in = new Input(input.text.value)
          //print("innnn")
          if (!XO.Controller(stat, in).getAction) println("Error Action")
          else {
            stat.setPlayer({
              if (stat.getPlayer == 1) 2 else 1
            })
            arr = engine.engine(XO.Drawer, XO.Controller, stat, in)
            stat = XO.Controller(stat, in)

            //(arr.array)
            for (i <- 0 until 3) {
              count += 1
              for (j <- 0 until 3) {
                val r = new Button()
                r.text = arr(i)(j)
                r.setPrefSize(60, 60)
                gridPane.add(r, j, i)

                count += 1
              }
            }
          }
          gridPane.layoutX = 150
          gridPane.layoutY = 150
          content = List(gridPane, label, input, ok, Reset, quiet)
        }
        }
      }
    }
      stage.scene = scene1
    }
  def printer (char:String): String ={
    var  sh:String=" ";
    char match {
      case "R" =>  sh=Character.toString(9814)
      case "N" =>  sh=Character.toString(9816)
      case "B" =>  sh=Character.toString(9815)
      case "Q" =>  sh= Character.toString(9813)
      case "K" =>  sh=Character.toString(9812)
      case "P" =>  sh=Character.toString(9817)
      case "r" =>  sh=Character.toString(9820)
      case "n" =>  sh=Character.toString(9822)
      case "b" =>  sh=Character.toString(9821)
      case "q" =>  sh=Character.toString(9819)
      case "k" =>  sh=Character.toString(9818)
      case "p" =>  sh=Character.toString(9823)
      case   _ =>  sh="  "
    }
    //print(sh)
    sh
  }

  }


