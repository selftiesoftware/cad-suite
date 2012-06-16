/*
 * Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.base.properties

import com.siigna.app.view.Graphics
import com.siigna._
import com.siigna.module.base.Menu
import java.awt.Color

/**
 * a wheel to select weights (and styles?) for lines
 *
 */

object Weight extends Module {

  var activeAngle : Double = 0

  var activeLine : Double = 0


  //TODO: make this dynamic so that each line is drawn by the same function, only changing the .
  //a list of the possible lines
  lazy val line0 = 0.00
  lazy val line30 = 0.09
  lazy val line60 = 0.18
  lazy val line90 = 0.25
  lazy val line120 = 0.30
  lazy val line150 = 0.40
  lazy val line180 = 0.60
  lazy val line210 = 0.80
  lazy val line240 = 1.00
  lazy val line270 = 1.50
  lazy val line300 = 2.00
  lazy val line330 = 3.00

  var relativeMousePosition : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None

  //PROPERTIES OF TEXT DESCRIPTIONS
  var boundingRectangle : Option[Rectangle2D] = None
  var text     = ""
  var scale    = 8
  var attributes = Attributes( "TextSize" -> 10)


  def eventHandler = EventHandler(stateMap, stateMachine)

  private var gotMouseDown = false

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

    'Start  ->   'KeyDown   ->  'End

  )
  def stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      Siigna.navigation = false // Make sure the rest of the program doesn't move
      eventParser.disable // Disable tracking and snapping
      startPoint = if (Menu.center.isDefined) Some(Menu.center.get) else Some(Vector2D(0,0))

      events match {
        case MouseMove(point, _, _) :: tail => relativeMousePosition = Some(point)
        case MouseDown(point, MouseButtonRight, _) :: tail => Goto('End)
        //selects the color to use
        case MouseDown(point, MouseButtonLeft, _) :: tail => {
          //catch first Mouse Down
          if (gotMouseDown == false) {
            relativeMousePosition = Some(point)
            gotMouseDown = true
          }
          //if the mouse has been pressed once, set the color and go to 'End.
          else {
            relativeMousePosition = Some(point)

            //set the Weight
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
          activeAngle = ((activeAngle + 15)/30).toInt * 30
      }
    }),
    'End -> ((events : List[Event]) => {
      //if no objects are selected, make the chosen lineWeight the default lineWeight
      if(Model.selection.isEmpty) {
        Siigna("activeLineWeight") = activeLine
      }
      //if a selection is defined, change lineweight of the selected shapes.
      else {
        Model.selection.foreach(s => s.setAttributes("StrokeWidth" -> activeLine))
      }

      //clear values and reactivate navigation
      gotMouseDown = false
      startPoint = None
      relativeMousePosition = None
      eventParser.enable
      Siigna.navigation = true
    })
  )
  override def paint(g : Graphics, transform : TransformationMatrix) = {
    if (startPoint.isDefined && relativeMousePosition.isDefined) {

      //template for lines
      def line(width : Double) = LineShape(Vector2D(47,100), Vector2D(-15,83)).setAttribute("StrokeWidth" -> width)

      val sp = startPoint.get.transform(transform)
      val t  = TransformationMatrix(sp,1.3)
      var i : Int = 0
      var weights : List[Double] = List(0.60,0.40,0.30,0.25,0.18,0.09,0.00,3.00,2.00,1.50,1.00,0.80,0.00)
      //function to rotate the graphics
      def drawLine (rotation : Int) {
          var activeLine = {
            LineShape(Vector2D(47,100), Vector2D(-15,83)).setAttributes("StrokeWidth" -> 4.0, "Color" -> new Color(0.75f, 0.75f, 0.75f, 0.80f))
          }
          //draw the active weight
          g draw activeLine.transform(t.rotate(activeAngle-180))
        }

      //TODO: add differentiated lineweight
      //draw the lines
      drawLine(0)
      drawLine(3)
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

      //draw an outline of the menu
      //g draw CircleShape(Vector2D(0,0), Vector2D(0,80)).transform(t)
      //g draw CircleShape(Vector2D(0,0), Vector2D(0,118)).transform(t)

      //draw the lines
      radians(30).foreach(radian => {
        i+=1
        g draw line(weights(i-1)).transform(t.rotate(radian))
      })

      //TODO: this is a hack! refactor.
      //draw a text description

      var text30 : TextShape = TextShape(line240.toString+" ", Vector2D(-89.48,66.83), scale, attributes)
      var text60 : TextShape = TextShape(line270.toString+" ", Vector2D(-112.2,16.74), scale, attributes)
      var text90 : TextShape = TextShape(line300.toString+" ", Vector2D(-106.8,-38.01), scale, attributes)
      var text120 : TextShape = TextShape(line330.toString+" ", Vector2D(-74.83,-82.75), scale, attributes)
      var text150 : TextShape = TextShape(line0.toString+" ", Vector2D(-24.74,-105.5), scale, attributes)
      var text180 : TextShape = TextShape(line30.toString+" ", Vector2D(30.01,-100.1), scale, attributes)
      var text210 : TextShape = TextShape(line60.toString+" ", Vector2D(74.75,-68.1), scale, attributes)
      var text240 : TextShape = TextShape(line90.toString+" ", Vector2D(97.48,-18.01), scale, attributes)
      var text270 : TextShape = TextShape(line120.toString+" ", Vector2D(92.12,36.74), scale, attributes)
      var text300 : TextShape = TextShape(line150.toString+" ", Vector2D(60.1,81.48), scale, attributes)
      var text330 : TextShape = TextShape(line180.toString+" ", Vector2D(10.01,104.2), scale, attributes)
      var text0 : TextShape = TextShape(line210.toString+" ", Vector2D(-44.74,98.85), scale, attributes)

      g draw text0.transform(t)
      g draw text30.transform(t)
      g draw text60.transform(t)
      g draw text90.transform(t)
      g draw text120.transform(t)
      g draw text150.transform(t)
      g draw text180.transform(t)
      g draw text210.transform(t)
      g draw text240.transform(t)
      g draw text270.transform(t)
      g draw text300.transform(t)
      g draw text330.transform(t)
    }
  }
}