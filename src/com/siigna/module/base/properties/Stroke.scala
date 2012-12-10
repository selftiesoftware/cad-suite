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
import java.awt.Color

/**
 * a wheel to select weights (and styles?) for lines
 *
 */

class Stroke extends Module {

  var activeLine : Option[Double] = None

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

  var startPoint : Vector2D = View.center

  //PROPERTIES OF TEXT DESCRIPTIONS
  var boundingRectangle : Option[Rectangle2D] = None
  var text     = ""
  var scale    = 8
  var attributes = Attributes( "TextSize" -> 10)

  //get the current angle from the mouse to the center of the line weight menu
  def getActiveAngle(startPoint : Vector2D, mousePosition : Vector2D) : Int = {
    var activeAngle = 0
    val radian = (mousePosition - startPoint).angle.toInt
    var calculatedAngle = radian + 450
    if (calculatedAngle > 360)
      activeAngle = calculatedAngle - 360
    else activeAngle = calculatedAngle
    activeAngle = ((activeAngle+24) /30).toInt * 30
    //return the angle
    activeAngle-180
  }

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

  def stateMap: StateMap = Map(
    //select a color
    'Start -> {
      case e => {
        Siigna.navigation = false // Make sure the rest of the program doesn't move
        eventParser.disable() // Disable tracking and snapping
        'Interaction
      }
    },
    'Interaction -> {

      case MouseDown(point, MouseButtonRight, _) :: tail => End

      //selects the color to use
      case MouseDown(point, MouseButtonLeft, _) :: tail => {

        var activeAngle = getActiveAngle(View.center,mousePosition)

        if (activeAngle == 180) {activeLine = Some(line0)}
        else if (activeAngle == 210) {activeLine = Some(line30)}
        else if (activeAngle == 24) {activeLine = Some(line60)}
        else if (activeAngle == 270) {activeLine = Some(line90)}
        else if (activeAngle == 300) {activeLine = Some(line120)}
        else if (activeAngle == 330) {activeLine = Some(line150)}
        else if (activeAngle ==   0) {activeLine = Some(line180)}
        else if (activeAngle ==  30) {activeLine = Some(line210)}
        else if (activeAngle ==  60) {activeLine = Some(line240)}
        else if (activeAngle ==  90) {activeLine = Some(line270)}
        else if (activeAngle == 120) {activeLine = Some(line300)}
        else if (activeAngle == 150) {activeLine = Some(line330)}

      }
      if(activeLine.isDefined) Siigna.activeLineWeight = activeLine.get
      //if a selection is defined, change lineweight of the selected shapes and deselect them.
      if(!Drawing.selection.isEmpty) {
        Drawing.selection.foreach(s => s.addAttributes("StrokeWidth" -> activeLine.get, "Color" -> Siigna.activeColor))
        Drawing.deselect()
      }

      //clear values and reactivate navigation
      eventParser.enable()
      Siigna.navigation = true
      End

    }
  )
  override def paint(g : Graphics, transform : TransformationMatrix) {

    //template for lines
    def line(width : Double) = LineShape(Vector2D(47,100), Vector2D(-15,83)).addAttribute("StrokeWidth" -> width)

    val t  = TransformationMatrix(startPoint,1.3)
    var i : Int = 0
    val weights : List[Double] = List(0.60,0.40,0.30,0.25,0.18,0.09,0.00,3.00,2.00,1.50,1.00,0.80,0.00)
    //function to rotate the graphics
    def drawLine (rotation : Int) {
        val activeLine = LineShape(Vector2D(47,100), Vector2D(-15,83)).addAttributes("StrokeWidth" -> 4.0, "Color" -> new Color(0.75f, 0.75f, 0.75f, 0.80f))
        //draw the active weight
        g draw activeLine.transform(t.rotate(getActiveAngle(View.center,mousePosition)))
      }

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
    //g draw CircleShape(View.center, Vector2D(View.center.x,80)).transform(t)
    //g draw CircleShape(View.center, Vector2D(View.center.x,118)).transform(t)

    //draw the lines
    radians(30).foreach(radian => {
      i+=1
      g draw line(weights(i-1)).transform(t.rotate(radian))
    })

    //draw a text description
    val text30 : TextShape = TextShape(line240.toString+" ", Vector2D(-89.48,66.83), scale, attributes)
    val text60 : TextShape = TextShape(line270.toString+" ", Vector2D(-112.2,16.74), scale, attributes)
    val text90 : TextShape = TextShape(line300.toString+" ", Vector2D(-106.8,-38.01), scale, attributes)
    val text120 : TextShape = TextShape(line330.toString+" ", Vector2D(-74.83,-82.75), scale, attributes)
    val text150 : TextShape = TextShape(line0.toString+" ", Vector2D(-24.74,-105.5), scale, attributes)
    val text180 : TextShape = TextShape(line30.toString+" ", Vector2D(30.01,-100.1), scale, attributes)
    val text210 : TextShape = TextShape(line60.toString+" ", Vector2D(74.75,-68.1), scale, attributes)
    val text240 : TextShape = TextShape(line90.toString+" ", Vector2D(97.48,-18.01), scale, attributes)
    val text270 : TextShape = TextShape(line120.toString+" ", Vector2D(92.12,36.74), scale, attributes)
    val text300 : TextShape = TextShape(line150.toString+" ", Vector2D(60.1,81.48), scale, attributes)
    val text330 : TextShape = TextShape(line180.toString+" ", Vector2D(10.01,104.2), scale, attributes)
    val text0 : TextShape = TextShape(line210.toString+" ", Vector2D(-44.74,98.85), scale, attributes)

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