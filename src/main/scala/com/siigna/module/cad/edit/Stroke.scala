/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.edit

import com.siigna.app.view.Graphics
import com.siigna._
import java.awt.Color

/**
 * a wheel to select weights (and styles?) for lines
 *
 */

class Stroke extends Module {

  var activeLine : Option[Double] = None
  var strokeColor : Color = new Color(0.0f, 0.0f, 0.0f, 1.00f)
  val transp = 1.00f

  def drawArc (dist : Int, span : Int, rotation : Int, color : Color, width : Double) : Shape = {
    val p1 = (View.center + Vector2D(0,dist)).rotate(View.center,rotation)
    val p2 = p1.rotate(View.center,-span)
    val p3 = p1.rotate(View.center,span)
    ArcShape(p2, p1, p3).addAttributes("StrokeWidth" -> width, "Color" -> color)
    //arc.transform(t.rotate(rotation))
    //draw the active weight
    //g draw activeArc.transform(t.rotate(getActiveAngle(View.center,mousePosition)))
  }

  lazy val noColor     = new Color(1.00f, 1.00f, 1.00f, transp)
  lazy val black       = new Color(0.00f, 0.00f, 0.00f, transp)
  lazy val grey        = new Color(0.60f, 0.60f, 0.60f, transp)
  lazy val yellow      = new Color(1.00f, 1.00f, 0.40f, transp)
  lazy val orange      = new Color(1.00f, 0.75f, 0.30f, transp)
  lazy val red         = new Color(0.95f, 0.12f, 0.30f, transp)
  lazy val magenta     = new Color(0.95f, 0.15f, 0.80f, transp)
  lazy val blue        = new Color(0.12f, 0.25f, 0.95f, transp)
  lazy val cyan        = new Color(0.10f, 0.95f, 0.95f, transp)
  lazy val green       = new Color(0.10f, 0.95f, 0.10f, transp)

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
  var boundingRectangle : Option[SimpleRectangle2D] = None
  var text     = ""
  var scale    = 8
  var attributes = Attributes( "TextSize" -> 10)

  //get the current angle from the mouse to the center of the line weight menu
  def getActiveAngle(startPoint : Vector2D, mousePosition : Vector2D) : Int = {
    var activeAngle = 0
    val radian = (mousePosition - startPoint).angle.toInt
    val calculatedAngle = radian + 450
    if (calculatedAngle > 360)
      activeAngle = calculatedAngle - 360
    else activeAngle = calculatedAngle
    activeAngle = ((activeAngle+24) /45).toInt * 45
    //return the angle
    activeAngle
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

        val activeAngle = getActiveAngle(View.center,mousePosition)
        val activeDist = View.center.distanceTo(mousePosition)


        if (activeAngle == 180)      {Siigna("activeLineWidth") = Some(0.1)}
        else if (activeAngle == 225) {Siigna("activeLineWidth") = Some(4.0)}
        else if (activeAngle == 270) {Siigna("activeLineWidth") = Some(2.0)}
        else if (activeAngle == 315) {Siigna("activeLineWidth") = Some(1.0)}
        else if (activeAngle == 360) {Siigna("activeLineWidth") = Some(0.6)}
        else if (activeAngle ==  45) {Siigna("activeLineWidth") = Some(0.4)}
        else if (activeAngle ==  90) {Siigna("activeLineWidth") = Some(0.3)}
        else if (activeAngle == 135) {Siigna("activeLineWidth") = Some(0.2)}


      }
      //if a selection is defined, change lineweight of the selected shapes and deselect them.
      if(!Drawing.selection.isEmpty) {
        Drawing.selection.addAttribute("StrokeWidth" -> Siigna.double("activeLineWidth").get)
        Drawing.deselect()
      }
      //clear values and reactivate navigation
      eventParser.enable()
      Siigna.navigation = true
      End
    }
  )
  override def paint(g : Graphics, transform : TransformationMatrix) {
    val w = Siigna.double("activeLineWidth").getOrElse(0.2)
    val dist = View.center.distanceTo(mousePosition)

    //draw stroke width options
    g draw drawArc(120,15,225,strokeColor,4.0)
    g draw drawArc(120,15,270,strokeColor,2.0)
    g draw drawArc(120,15,315,strokeColor,1.0)
    g draw drawArc(120,15,0,strokeColor,0.6)
    g draw drawArc(120,15,45,strokeColor,0.4)
    g draw drawArc(120,15,90,strokeColor,0.3)
    g draw drawArc(120,15,135,strokeColor,0.2)
    g draw drawArc(120,15,180,strokeColor,0.1)

    //draw stroke color options
    g draw drawArc(90,15,225,yellow,w)
    g draw drawArc(90,15,270,green,w)
    g draw drawArc(90,15,315,cyan,w)
    g draw drawArc(90,15,0,blue,w)
    g draw drawArc(90,15,45,magenta,w)
    g draw drawArc(90,15,90,red,w)
    g draw drawArc(90,15,135,grey,w)
    g draw drawArc(90,15,180,black,w)

    //draw stroke type options
    g draw drawArc(60,30,180,strokeColor,w)
    g draw drawArc(60,30,270,strokeColor,w)
    g draw drawArc(60,30,0,strokeColor,w)
    g draw drawArc(60,30,90,strokeColor,w)

    //draw active option
    val a = getActiveAngle(mousePosition,View.center)
    if(dist > 100) g draw drawArc(120,18, -a,new Color(0.00f, 0.00f, 0.00f, 0.50f),6.0)
    else if(dist < 70) g draw drawArc(60,18,-a,new Color(0.00f, 0.00f, 0.00f, 0.50f),3.0)
    else g draw drawArc(90,18,-a,strokeColor,3.0)

    //draw an outline of the menu
    //g draw CircleShape(View.center, Vector2D(View.center.x,80)).transform(t)
    //g draw CircleShape(View.center, Vector2D(View.center.x,118)).transform(t)

    //draw the lines
    //radians(30).foreach(radian => {
    //  i+=1
      //g draw line(weights(i-1)).transform(t.rotate(radian))
    //})

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

    //g draw text0.transform(t)
    //g draw text30.transform(t)
    //g draw text60.transform(t)
    //g draw text90.transform(t)
    //g draw text120.transform(t)
    //g draw text150.transform(t)
    //g draw text180.transform(t)
    //g draw text210.transform(t)
    //g draw text240.transform(t)
    //g draw text270.transform(t)
    //g draw text300.transform(t)
    //g draw text330.transform(t)
  }
}