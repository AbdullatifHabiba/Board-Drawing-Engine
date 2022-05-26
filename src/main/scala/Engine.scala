import play.stage
import scalafx.scene.control.{Alert, Button, Label, TextArea, TextField}
import scalafx.Includes._
import scalafx.Includes.when
import scalafx.scene.Scene
import scalafx.application.JFXApp3
import scalafx.beans.property.StringProperty
import scalafx.event.ActionEvent
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.effect.BlendMode.{Blue, Green, Red}
import scalafx.scene.image.Image
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{Background, BackgroundFill, CornerRadii}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.{Brown, Chocolate, Coral, DARKVIOLET, LightGray}
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.GridPane.{getColumnIndex, getRowIndex}
import scalafx.scene.layout.Priority
import scalafx.scene.shape.{Circle, Rectangle}
import scalafx.scene.text.Font
import scalafx.scene.web.WebEvent.Alert

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
      title = "Engine games"

    }
    val scene1 = new Scene(500, 500) {

      fill =DARKVIOLET


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
        quiet.style = "-fx-background-color: blue"

        content.foreach(p => {
          p.disable = false
          p.style = "-fx-background-color: gold"
        })
        quiet.style = "-fx-background-color: red"
        Reset.style = "-fx-background-color: green"

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
            r.text = arr(7 - i)(7 - j)
            // r.style = "-fx-font: 30 arial;"
            r.setPrefSize(50, 50)
            if (count % 2 == 0) r.style = "-fx-background-color: black;" else r.style = "-fx-background-color: white;"
            gridPane.add(r, j, i)


            count += 1
          }
        }
        content = List(gridPane, label, input, ok,Reset,quiet)

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
            //  print(arr.array)
            for (i <- 0 until 8) {
              count += 1
              for (j <- 0 until 8) {
                val r = new Button()
                r.text = arr(7 - i)(7 - j)
                r.setPrefSize(50, 50)
                if (count % 2 == 0) r.style = "-fx-background-color: black;" else r.style = "-fx-background-color: white;"
                gridPane.add(r, j, i)
                gridPane.layoutX = 50
                gridPane.layoutY = 30
                count += 1
              }
            }
          }

          content = List(gridPane, label, input, ok,Reset,quiet)
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
        val s = 100
        for (i <- 0 until 8) {
          count += 1
          for (j <- 0 until 8) {
            val r = new Button()

            r.setPrefSize(50, 50)
            if (count % 2 == 0) r.style = "-fx-background-color: black;-fx-font: 20 arial;" else r.style = "-fx-background-color: white;-fx-font: 20 arial;"
            r.text = arr(i)(j)

            gridPane.add(r, j, i)


            count += 1
          }
        }
        content = List(gridPane, label, input, ok,Reset,quiet)

      }

        ok.onMouseClicked = { (e) => {
          stat = checker.Controller(stat, in)
          in = new Input(input.text.value)
          if (!checker.Controller(stat, in).getAction) println("Error Action")
          else {
            stat.setPlayer({
              if (stat.getPlayer == 1) 2 else 1
            })
            arr = engine.engine(checker.Drawer, checker.Controller, stat, in)
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
          }

          content = List(gridPane, label, input, ok,Reset,quiet)
        }
        }
      }
      Reset.onMouseClicked = { (e) => {

        quiet.style = "-fx-background-color: red"
        Reset.style = "-fx-background-color: green"
        content = List(gridPane, label, input, ok,Reset,quiet)

      }
      }
    }
    stage.scene = scene1
  }


}

