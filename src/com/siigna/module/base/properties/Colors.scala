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
import com.siigna.module.base.Menu

/**
 * a wheel to select colors for lines, surfaces and text
 * //TODO: draw the color wheel where the radial menu center is instead centered around the last mouse click.
 * //TODO add color selection functionality
   //TODO: the color wheel only shows after mouse move???
 */

class Colors extends Module {

  var activeColor: Option[Color] = None
  
  //colors, inspired by crayola crayons: http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors
  lazy val black       = new Color(0.00f, 0.00f, 0.00f, transp)
  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, transp)
  lazy val dimgrey     = new Color(0.40f, 0.40f, 0.40f, transp)
  lazy val grey        = new Color(0.60f, 0.60f, 0.60f, transp)
  lazy val darkGrey    = new Color(0.75f, 0.75f, 0.75f, transp)
  lazy val lightGrey   = new Color(0.90f, 0.90f, 0.90f, transp)
  lazy val yellow      = new Color(1.00f, 1.00f, 0.40f, transp)
  lazy val orange      = new Color(1.00f, 0.75f, 0.30f, transp)
  lazy val orangeRed   = new Color(0.95f, 0.45f, 0.22f, transp)
  lazy val red         = new Color(0.95f, 0.12f, 0.30f, transp)
  lazy val radicalRed  = new Color(0.95f, 0.14f, 0.46f, transp)
  lazy val violetRed   = new Color(0.95f, 0.15f, 0.58f, transp)
  lazy val magenta     = new Color(0.95f, 0.15f, 0.80f, transp)
  lazy val plum        = new Color(0.64f, 0.18f, 0.85f, transp)
  lazy val purple      = new Color(0.35f, 0.22f, 0.90f, transp)
  lazy val blue        = new Color(0.12f, 0.25f, 0.95f, transp)
  lazy val navyBlue    = new Color(0.10f, 0.45f, 0.95f, transp)
  lazy val pasificBlue = new Color(0.10f, 0.65f, 0.95f, transp)
  lazy val cyan        = new Color(0.10f, 0.95f, 0.95f, transp)
  lazy val turquise    = new Color(0.10f, 0.95f, 0.75f, transp)
  lazy val caribbean   = new Color(0.10f, 0.95f, 0.50f, transp)
  lazy val green       = new Color(0.10f, 0.95f, 0.10f, transp)
  lazy val lime        = new Color(0.50f, 0.95f, 0.15f, transp)
  lazy val yellowGreen = new Color(0.65f, 0.95f, 0.15f, transp)

  //get the current angle from the mouse to the center of the color wheel
  def getActiveAngle(startPoint : Vector2D, mousePosition : Vector2D) : Int = {
    var activeAngle = 0
    val radian = (mousePosition - startPoint).angle.toInt
    var calculatedAngle = radian * -1 + 450
    if (calculatedAngle > 360)
      activeAngle = calculatedAngle - 360
    else activeAngle = calculatedAngle
      activeAngle = ((activeAngle + 7.5)/15).toInt * 15
    //return the angle
    activeAngle
  }
    
  //make a list of radians- used to populate color slots in the color wheel.
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

  var relativeMousePosition : Option[Vector2D] = None 
  var startPoint : Option[Vector2D] = None
  private val transp = 1.00f

  def stateMap: StateMap = Map(
    //select a color
  'Start -> {
    case e => { 
      println("in colors")
      Siigna.navigation = false // Make sure the rest of the program doesn't move
      eventParser.disable() // Disable tracking and snapping
      'Interaction
    }  
  },
    'Interaction -> {
      case MouseMove(p, _, _) :: tail => {
        relativeMousePosition = Some(p)
        println(relativeMousePosition)
        startPoint = Some(Vector2D(0,0))
      }
      
      case MouseDown(p, MouseButtonLeft, _) :: tail => {
        if(Drawing.selection.isEmpty) {
          if(activeColor.isDefined) Siigna("activeColor") = activeColor.get
        }
        //if a selection is defined, change lineweight of the selected shapes and deselect them.
        else {
          Drawing.selection.foreach(s => s.addAttribute("Color" -> activeColor.get))
          Drawing.deselect()
        }
        //clear values and reactivate navigation
        println("INTERACT!")
        eventParser.enable()
        Siigna.navigation = true
        ModuleEnd
      }
    }
  )

  override def paint(g : Graphics, transform : TransformationMatrix) {
    if (startPoint.isDefined && relativeMousePosition.isDefined) {
      //centerPoint for the colorWheel
      val sp = startPoint.get.transform(transform)
      val t  = TransformationMatrix(sp,1.3)

      lazy val colorFill = Array(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76),Vector2D(0,76),Vector2D(2.47,76),Vector2D(6.52,75.7),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,98.9),Vector2D(0,99),Vector2D(-4.35,98.9),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
      lazy val colorIcon = PolylineShape(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76.2),Vector2D(0,76.4),Vector2D(2.47,76.2),Vector2D(6.52,75.8),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,99.01),Vector2D(0,99.2),Vector2D(-4.35,99.1),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
      lazy val colorActive = PolylineShape(Vector2D(-10.93,73.5),Vector2D(-7.96,73.8),Vector2D(-4.55,74),Vector2D(-2,74),Vector2D(4.47,74),Vector2D(8.52,73.7),Vector2D(10.93,73.5),Vector2D(13.9,100.3),Vector2D(11.06,100.6),Vector2D(6.35,100.9),Vector2D(-2,101),Vector2D(-6.35,100.9),Vector2D(-11.04,100.6),Vector2D(-13.9,100.3),Vector2D(-10.93,73.5))

      def drawFill (color : Color, rotation : Int) {

        val fillVector2Ds = colorFill.map(_.transform(t.rotate(rotation)))
        val fillScreenX = fillVector2Ds.map(_.x.toInt).toArray
        val fillScreenY = fillVector2Ds.map(_.y.toInt).toArray

        g setColor color

        g.g.fillPolygon(fillScreenX, fillScreenY, fillVector2Ds.size)
      }

      // Draw the color icons
      drawFill(magenta,0)
      drawFill(violetRed,15)
      drawFill(radicalRed,30)
      drawFill(red,45)
      drawFill(orangeRed,60)
      drawFill(orange,75)
      drawFill(yellow,90)
      drawFill(lightGrey,105)
      drawFill(darkGrey,120)
      drawFill(grey,135)
      drawFill(dimgrey,150)
      drawFill(anthracite,165)
      drawFill(black,180)
      drawFill(yellowGreen,195)
      drawFill(lime,210)
      drawFill(green,225)
      drawFill(caribbean,240)
      drawFill(turquise,255)
      drawFill(cyan,270)
      drawFill(pasificBlue,285)
      drawFill(navyBlue,300)
      drawFill(blue,315)
      drawFill(purple,330)
      drawFill(plum,345)

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