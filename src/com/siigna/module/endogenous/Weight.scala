package com.siigna.module.endogenous

import com.siigna.app.view.Graphics
import com.siigna._
import java.awt.Color
import radialmenu.{RadialMenuIcon, MenuEvent, MenuItem}

/**
 * a wheel to select colors for lines, surfaces and text
 * //TODO: draw the color wheel where the radial menu center is instead centered around the last mouse click.
 * //TODO add color selection functionality
   //TODO: the color wheel only shows after mouse move???
 */

object Weight extends Module {

  var activeAngle : Double = 0
  var relativeMousePosition : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None

  var activeColor: Option[Color] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

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
          relativeMousePosition = Some(point)


          Goto('End)
        }
        case _ =>
      }
      //get the current angle from the mouse to the center of the color wheel
      if (relativeMousePosition.isDefined && startPoint.isDefined) {
        val radian = (relativeMousePosition.get - startPoint.get).angle.toInt
        var calculatedAngle = radian * -1 + 450
        if (calculatedAngle > 360)
          activeAngle = calculatedAngle - 360
        else activeAngle = calculatedAngle
          activeAngle = ((activeAngle +7.5)/15).toInt * 15
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

      lazy val colorFill = Array(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76),Vector2D(0,76),Vector2D(2.47,76),Vector2D(6.52,75.7),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,98.9),Vector2D(0,99),Vector2D(-4.35,98.9),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
      lazy val colorIcon = PolylineShape.fromPoints(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76.2),Vector2D(0,76.4),Vector2D(2.47,76.2),Vector2D(6.52,75.8),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,99.01),Vector2D(0,99.2),Vector2D(-4.35,99.1),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
      lazy val colorActive = PolylineShape.fromPoints(Vector2D(-10.93,73.5),Vector2D(-7.96,73.8),Vector2D(-4.55,74),Vector2D(-2,74),Vector2D(4.47,74),Vector2D(8.52,73.7),Vector2D(10.93,73.5),Vector2D(13.9,100.3),Vector2D(11.06,100.6),Vector2D(6.35,100.9),Vector2D(-2,101),Vector2D(-6.35,100.9),Vector2D(-11.04,100.6),Vector2D(-13.9,100.3),Vector2D(-10.93,73.5))

      def drawFill (color : Color, rotation : Int) {

        val fillVector2Ds = colorFill.map(_.transform(t.rotate(rotation)))
        val fillScreenX = fillVector2Ds.map(_.x.toInt).toArray
        val fillScreenY = fillVector2Ds.map(_.y.toInt).toArray

        g setColor color

        g.g.fillPolygon(fillScreenX, fillScreenY, fillVector2Ds.size)
      }

      // Draw the color icons
      drawFill(zero,0)
      drawFill(pointOhNine,345)
      drawFill(pointOneFive,330)
      drawFill(pointOneEight,315)
      drawFill(pointTwo,300)
      drawFill(pointTwoFive,285)
      drawFill(pointThree,270)
      drawFill(pointThreeFive,255)
      drawFill(pointFour,240)

      //draw a border around the active color
      val distanceToCentre = startPoint.get - relativeMousePosition.get

      if (distanceToCentre.length > 80 && distanceToCentre.length < 130 )  {

      //g draw colorIcon.transform(t.rotate(activeAngle-180))
      //g draw colorActive.transform(t.rotate(activeAngle-180))

      }
      //draw the color wheel icon outlines
      radians(45).foreach(radian => { g draw colorIcon.transform(t.rotate(radian))})
    }
  }
}