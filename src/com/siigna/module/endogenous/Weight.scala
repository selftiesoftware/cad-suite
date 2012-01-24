package com.siigna.module.endogenous

import com.siigna.app.view.Graphics
import com.siigna._
import java.awt.Color
import radialmenu.{RadialMenuIcon, MenuEvent, MenuItem}

/**
 * a wheel to select weights (and styles?) for lines
 *
 */

object Weight extends Module {

  var activeAngle : Double = 0

  lazy val line0 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line30 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line60 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line90 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line120 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line150 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line180 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line210 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line240 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line270 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line300 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))
  lazy val line330 = PolylineShape.fromPoints(Vector2D(-9.93,75.5),Vector2D(-6.96,75.8),Vector2D(-3.55,76.2),Vector2D(-1,76.4))


  var relativeMousePosition : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None

  var activeLine : PolylineShape = PolylineShape.empty

  def eventHandler = EventHandler(stateMap, stateMachine)

  private var gotMouseDown = false

  val line = LineShape(Vector2D(20,105),Vector2D(-20,85))

  def radians(rotation : Int) : List[Int] = {

    var i = 0
    var activeRadians = List[Int]()

    //add all radians to a list
    do {

      activeRadians = i :: activeRadians
      i += rotation

    } while (i <= 360)
    activeRadians
  }

  def stateMap = DirectedGraph(

    'Start  ->   'KeyEscape   ->  'End

  )

  def stateMachine = Map(
    //select a color
    'Start -> ((events : List[Event]) => {
      Siigna.navigation = false // Make sure the rest of the program doesn't move
      eventParser.disable // Disable tracking and snapping
      events match {
        case MouseUp(point, _, _) :: tail => startPoint = Some(Menu.oldCenter)
        case MouseMove(point, _, _) :: tail => relativeMousePosition = Some(point)
        //selects the color to use
        case MouseDown(point, _, _) :: tail => {
          //if the mouse has been pressed once, set the color and go to 'End.
          if (gotMouseDown == true) {
            relativeMousePosition = Some(point)
            //set the color
            if (activeAngle == 0) {activeLine = line0}
            else if (activeAngle == 30) {activeLine = line30}
            else if (activeAngle == 60) {activeLine = line60}
            else if (activeAngle == 90) {activeLine = line90}
            else if (activeAngle == 120) {activeLine = line120}
            else if (activeAngle == 150) {activeLine = line150}
            else if (activeAngle == 180) {activeLine = line180}
            else if (activeAngle == 210) {activeLine = line210}
            else if (activeAngle == 240) {activeLine = line240}
            else if (activeAngle == 270) {activeLine = line270}
            else if (activeAngle == 300) {activeLine = line300}
            else if (activeAngle == 330) {activeLine = line330}
            gotMouseDown = false
            Goto('End)
          }

          else {
            //catch the first mouse down
            startPoint = Some(Menu.oldCenter)
            relativeMousePosition = Some(point)
            gotMouseDown = true
          }
        }
        case _ =>
      }
      //get the current angle from the mouse to the center of the line weight menu
      if (relativeMousePosition.isDefined && startPoint.isDefined) {
        val radian = (relativeMousePosition.get - startPoint.get).angle.toInt
        var calculatedAngle = radian * -1 + 450
        if (calculatedAngle > 360)
          activeAngle = calculatedAngle - 360
        else activeAngle = calculatedAngle
          activeAngle = ((activeAngle +7.5)/30).toInt * 30
      }
    }),
    'End -> ((events : List[Event]) => {
      //clear values and reactivate navigation
      startPoint = None
      relativeMousePosition = None
      eventParser.enable
      Siigna.navigation = true
    })
  )
  override def paint(g : Graphics, transform : TransformationMatrix) = {
    if (startPoint.isDefined && relativeMousePosition.isDefined) {
      val sp = startPoint.get.transform(transform)
      val t  = TransformationMatrix(sp,1.3)

      def drawLine (rotation : Int) {

          g draw line.transform(t.rotate(activeAngle-180))
        }

      //TODO: add differentiated lineweight
      //draw the lines
      drawLine(0)
      drawLine(30)
      drawLine(60)
      drawLine(90)
      drawLine(120)
      drawLine(150)
      drawLine(180)
      drawLine(210)
      drawLine(240)
      drawLine(270)
      drawLine(300)
      drawLine(330)

      //draw the lines
      radians(30).foreach(radian => { g draw line.transform(t.rotate(radian))})

    }
  }
}