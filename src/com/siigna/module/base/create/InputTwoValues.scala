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
    if (coordinateValue.length > 0 ) {
      //If there is an entered X-value:
      var x: Option[String] = None

      if (!xCoord.isEmpty) {
        if (xCoord.get % 1 == 0) x = Some(xCoord.get.toInt.toString)
        else x = Some(xCoord.get.toString)
      } else if (value == "-") x = Some(value)
        else if (((java.lang.Double.parseDouble(value))*100)%1 != 0) {
        x = Some(coordinateValue.substring(0, coordinateValue.length-1))
        coordinateValue = x.get
      }
        else x = Some(value)

      println("Value: " +value)
      println("x: " + x)
      println("Xkoord: " + xCoord)
      //If there is an entered y-value:
      var y: Option[String] = None
      if (!xCoord.isEmpty && value != "-") {
        if ((((java.lang.Double.parseDouble(value))*100)%1) != 0) {
          println("her")
        y = Some(coordinateValue.substring(0, coordinateValue.length-1))
        coordinateValue = y.get
        } else y = Some(value)
      }
      else if (!xCoord.isEmpty && value == "-") y = Some(value)
      println("y: " +y)
      
      if (!x.isEmpty && y.isEmpty) ("point (X: " + x.get)
      else if (!y.isEmpty || y.get.length() == 0) ("point (X: "+x.get+", Y: " + y.get + ").")
      //Here must be something - at least a string eith a space -
      // as long as an empty string makes the whole thing go down...
      else "point (X: "
      //If the coord string is empty(y-coord deleted, or not entered yet:)
    } else if (coordinateValue.length() == 0) {
      val x: Option[String] = Some(if (!xCoord.isEmpty) {
        if (xCoord.get % 1 == 0) xCoord.get.toInt.toString
        else xCoord.get.toString
      } else value)
      if (!x.isEmpty && x.get.length() > 0) ("point (X: " + x.get + ", Y:")
      else ("point (X: " + x.get)
    }
    //Also here - at least a space
    else " "
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

      //Read numbers and minus, after drawing of guide:
      case Start(_ ,g: PointGuide) :: KeyDown(code, _) :: tail => {
        pointGuide = Some(g.guide)
        startPoint = Some(g.point)
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        //if a comma is the first thing entered, it is interpreted as zero:
        if (code.toChar.toString == "," && coordinateValue.length() == 0) {
          coordinateX = Some(0.0)
        }
        Siigna display message(coordinateValue, coordinateX, coordinateY)
      }

    //goto second coordinate if ENTER, COMMA, or TAB is pressed
    case KeyDown(Key.Enter | Key.Tab | (','), _) :: tail => {
      //when ENTER, "," or TAB is pressed, and a value is set, this value is passed as the first coordinate relative to 0,0
      if (coordinateX.isEmpty && coordinateValue.length > 0) {
        coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))
        if (((coordinateX.get * 100) % 1) != 0) coordinateX = Some((math.floor(coordinateX.get * 100))/100)
        else if (((coordinateX.get * 10) % 1) == 0) coordinateX = Some((math.floor(coordinateX.get * 10))/10)
        coordinateValue = ""
        Siigna display message(coordinateValue, coordinateX, coordinateY)
      }
      //If X is already entered, it is interpreted as Y, and the module ends:
      else if (coordinateY.isEmpty && coordinateValue.length > 0) {
        coordinateY = Some(java.lang.Double.parseDouble(coordinateValue))
        if (((coordinateY.get * 100) % 1) == 0) coordinateY = Some((math.floor(coordinateY.get * 100))/100)
        else if (((coordinateY.get * 10) % 1) == 0) coordinateY = Some((math.floor(coordinateY.get * 10))/10)
        coordinateValue = ""
        //if a full set of coordinates are present, return them to the calling module.
        if(startPoint.isDefined) {
          End(Vector2D(startPoint.get.x + x,startPoint.get.y + y))
        }
        else End(Vector2D(x,y))
      }
    }
    case KeyDown(Key.Backspace, _) :: tail => {
      //If there ia an active coordinate string, it is reduced by one and displayed:
      if (coordinateValue.length > 0) {
        coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
        Siigna display message(coordinateValue, coordinateX, coordinateY)
      }
      //If there is no active string, but an active x-coordinate,
      else if (coordinateX.isDefined) {
        if ((coordinateX.get % 1) == 0.0) coordinateValue = coordinateX.get.toInt.toString
        else coordinateValue = coordinateX.get.toString
        coordinateX     = None
        Siigna display message(coordinateValue, coordinateX, coordinateY)
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