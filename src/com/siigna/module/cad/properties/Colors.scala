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

package com.siigna.module.cad.properties

import com.siigna._
import java.awt.Color
/**
 * a wheel to select colors for lines, surfaces and text
 */

class Colors extends Module {

  var activeColor: Option[Color] = None

  var angle : Int = 0
  
  var centerPoint : Vector2D = View.center

  //colors, inspired by crayola crayons: http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors
  lazy val black       = new Color(0.00f, 0.00f, 0.00f, transp)
  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, transp)
  lazy val dimgrey     = new Color(0.40f, 0.40f, 0.40f, transp) //does not work
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
    val calculatedAngle = radian + 450
    if (calculatedAngle > 360)
      activeAngle = calculatedAngle - 360
    else activeAngle = calculatedAngle
      activeAngle = ((activeAngle + 7.5)/15).toInt * 15
    //return the angle
    activeAngle-180
  }
    
  //make a list of radians - used in paint to draw outlines around primary colors.
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

  private val transp = 1.00f

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
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case MouseDown(p, MouseButtonLeft, _) :: tail => {

        if (angle == 180) {activeColor = Some(black)}
        else if (angle == 195) activeColor = Some(yellowGreen)
        else if (angle == 210) activeColor = Some(lime)
        else if (angle == 225) activeColor = Some(green)
        else if (angle == 240) activeColor = Some(caribbean)
        else if (angle == 255) activeColor = Some(turquise)
        else if (angle == 270) activeColor = Some(cyan)
        else if (angle == 385) activeColor = Some(pasificBlue)
        else if (angle == 300) activeColor = Some(navyBlue)
        else if (angle == 315) activeColor = Some(blue)
        else if (angle == 330) activeColor = Some(purple)
        else if (angle == 345) activeColor = Some(plum)
        else if (angle ==   0) activeColor = Some(magenta)
        else if (angle ==  15) activeColor = Some(violetRed)
        else if (angle ==  30) activeColor = Some(radicalRed)
        else if (angle ==  45) activeColor = Some(red)
        else if (angle ==  60) activeColor = Some(orangeRed)
        else if (angle ==  75) activeColor = Some(orange)
        else if (angle ==  90) activeColor = Some(yellow)
        else if (angle == 105) activeColor = Some(lightGrey)
        else if (angle == 120) activeColor = Some(darkGrey)
        else if (angle == 135) activeColor = Some(grey)
        else if (angle == 150) activeColor = Some(dimgrey)
        else if (angle == 165) activeColor = Some(anthracite)

        if(activeColor.isDefined) {
          Siigna("activeColor") = activeColor.get
        }

        if(!Drawing.isEmpty) {
          Drawing.selection.addAttribute("Color" -> activeColor.get)
          Drawing.deselect()
        }
        //clear values and reactivate navigation
        eventParser.enable()
        Siigna.navigation = true
        End
      }
    }
  )

  override def paint(g : Graphics, transform : TransformationMatrix) {
    //centerPoint for the colorWheel

    val t  = TransformationMatrix(centerPoint,1.3)

    lazy val colorFill = Array(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76),Vector2D(0,76),Vector2D(2.47,76),Vector2D(6.52,75.7),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,98.9),Vector2D(0,99),Vector2D(-4.35,98.9),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
    lazy val colorIcon = PolylineShape(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76.2),Vector2D(0,76.4),Vector2D(2.47,76.2),Vector2D(6.52,75.8),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,99.01),Vector2D(0,99.2),Vector2D(-4.35,99.1),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
    lazy val colorActive = PolylineShape(Vector2D(-10.93,73.5),Vector2D(-7.96,73.8),Vector2D(-4.55,74),Vector2D(-2,74),Vector2D(4.47,74),Vector2D(8.52,73.7),Vector2D(10.93,73.5),Vector2D(13.9,100.3),Vector2D(11.06,100.6),Vector2D(6.35,100.9),Vector2D(-2,101),Vector2D(-6.35,100.9),Vector2D(-11.04,100.6),Vector2D(-13.9,100.3),Vector2D(-10.93,73.5))

    def drawFill (color : Color, rotation : Int) {

      val fillVector2Ds = colorFill.map(_.transform(t.rotate(rotation)))
      val fillScreenX = fillVector2Ds.map(_.x.toInt).toArray
      val fillScreenY = fillVector2Ds.map(_.y.toInt).toArray

      g setColor color

      g.AWTGraphics.fillPolygon(fillScreenX, fillScreenY, fillVector2Ds.size)
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
    val distanceToCentre = centerPoint - mousePosition

    //call the angle function to get the active angle, and store it in a var.
    angle = getActiveAngle(centerPoint, mousePosition)

    if (distanceToCentre.length > 80 && distanceToCentre.length < 130 )  {

      //calculate where to draw a box highlighting the currently active color
      g draw colorIcon.transform(t.rotate(angle))
      g draw colorActive.transform(t.rotate(angle))

    }
    //draw the color wheel icon outlines
    radians(45).foreach(radian => { g draw colorIcon.transform(t.rotate(radian))})
  }
}