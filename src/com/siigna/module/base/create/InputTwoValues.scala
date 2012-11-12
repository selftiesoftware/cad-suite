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

package com.siigna.module.base.create

import com.siigna._
import app.Siigna

/**
  * A module that collect a pair of digits on the basis of key inputs.
  * Used by modules that need eg. an X and Y coordinate to define a point.
*/

class InputTwoValues extends Module {

  private var coordinateX : Option[Double] = None   //text input for X values
  private var coordinateY : Option[Double] = None   //text input for Y values
  private var coordinateValue : String = ""  //input string for distances

  def message(value : String, xCoord : Option[Double], yCoord : Option[Double]) : String = {
    println(coordinateValue.length)
    if (coordinateValue.length > 0 ) {

      val x = if (xCoord.isDefined) "%.2f" format xCoord.get
      else value
      val y = if (yCoord.isDefined) "%.2f" format yCoord.get
      else if (xCoord.isDefined) value
      else ""

      ("point (X: "+x+", Y: "+y+").")

      //Display the coords when typed.
    } else if (xCoord.isDefined) {
      ("point (X: "+x+")")
    }
    else {
      ""
    }
  }

  var pointGuide : Option[Vector2D => Traversable[Shape]] = None
  var startPoint : Option[Vector2D] = None

  var relativeX : Double = 0.0
  var relativeY : Double = 0.0

  // Save the X value, if any
  def x : Double = if (!coordinateX.isEmpty)
    coordinateX.get
  else if (coordinateValue.length > 0 && coordinateValue != "-")
    java.lang.Double.parseDouble(coordinateValue)
  else 0.0

  // Save the Y value, if any
  def y : Double = if (coordinateY.isDefined)
    coordinateY.get
  else if (coordinateX.isDefined && coordinateValue.length > 0 && coordinateValue != "-")
    java.lang.Double.parseDouble(coordinateValue)
  else 0.0

  val stateMap: StateMap = Map(

    'Start -> {
    //goto second coordinate if ENTER, COMMA, or TAB is pressed
      case Start(_ ,g: PointGuide) :: KeyDown(code, _) :: tail => {
        pointGuide = Some(g.guide)
        startPoint = Some(g.point)
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar

        Siigna display message(coordinateValue, coordinateX, coordinateY)
      }

    case KeyDown(Key.Enter | Key.Tab | (','), _) :: tail => {
      //if noting is entered
      if (coordinateX.isEmpty && coordinateValue.length == 0) End
      //when ENTER is pressed, and a value is set, this value is passed as the first coordinate relative to 0,0
      else if (coordinateX.isEmpty && coordinateValue.length > 0) {
        coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))

        coordinateValue = ""
      }
      else if (coordinateY.isEmpty && coordinateValue.length > 0) {
        coordinateY = Some(java.lang.Double.parseDouble(coordinateValue))
        coordinateValue = ""
        //if a full set of coordinates are present, return them to the calling module.
        if(startPoint.isDefined) {
          End(Vector2D(startPoint.get.x + x,startPoint.get.y + y))
        }
        else End(Vector2D(x,y))
      }
    }
    case KeyDown(Key.Backspace, _) :: tail => {
      if (coordinateValue.length > 0) {
        coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
        Siigna display message(coordinateValue, coordinateX, coordinateY)

      }
      else if (coordinateX.isDefined) {
        coordinateValue = coordinateX.get.toString
        coordinateX     = None
      }
    }
    //if point returns a keyDown - that is not previously intercepted
    case KeyDown(code, _) :: tail => {
      //get the input from the keyboard if it is numbers, (-) or (.)
      val char = code.toChar
      if (char.isDigit)
        coordinateValue += char
      else if ((char == '.') && !coordinateValue.contains('.'))
        coordinateValue += "."
      else if (char == '-' && coordinateValue.length < 1)
        coordinateValue = "-"

      //Display the message
      Siigna display message(coordinateValue, coordinateX, coordinateY)
    }
    case _ => {
    }
  })
  override def paint(g : Graphics, t: TransformationMatrix) {
    //if points are in the process of being typed, then draw the shape dynamically on the basis of what coords are given.
    if(pointGuide.isDefined && startPoint.isDefined){
      relativeX = startPoint.get.x + x
      relativeY = startPoint.get.y + y
      pointGuide.foreach(_(Vector2D(startPoint.get.x + x,startPoint.get.y + y)).foreach(s => g.draw(s.transform(t))))
    }
  }
}