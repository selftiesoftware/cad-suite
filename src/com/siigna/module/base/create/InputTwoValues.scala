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
      case Start(_ ,g: PointGuide) :: tail => {
        pointGuide = Some(g.guide)
        startPoint = Some(g.point)
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
      if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
      else if (coordinateX.isDefined) {
        coordinateValue = coordinateX.get.toString
        coordinateX     = None
      }
    }
    //if point returns a keyDown - that is not previously intercepted
    case KeyDown(code, _) :: tail => {
      println("key in in COORD mod.")
      //get the input from the keyboard if it is numbers, (-) or (.)
      val char = code.toChar
      if (char.isDigit)
        coordinateValue += char
      else if ((char == '.') && !coordinateValue.contains('.'))
        coordinateValue += "."
      else if (char == '-' && coordinateValue.length < 1)
        coordinateValue = "-"

      val message = {
        if (coordinateValue.length > 0 ) {

          val x = if (coordinateX.isDefined) "%.2f" format coordinateX.get
          else coordinateValue
          val y = if (coordinateY.isDefined) "%.2f" format coordinateY.get
          else if (coordinateX.isDefined) coordinateValue
          else ""

          Some("point (X: "+x+", Y: "+y+").")

          //Display the coords when typed.
        } else if (coordinateX.isDefined) {
          Some("point (X: "+x+")")
        }
        else {
          None
        }
      }

      //Display the message
      if(message.isDefined) Siigna display(message.get)
    }
    case _ => {
    }
  })
  override def paint(g : Graphics, t: TransformationMatrix) {

    println("X: "+x)
    println("Y: "+y)

    println("relativeX: "+relativeX)
    println("relativeY: "+relativeY)

    println("stPoint: "+startPoint.get)

    //if points are in the process of being typed, then reformat the value to View coordinates..

    if(pointGuide.isDefined && startPoint.isDefined){
      relativeX = startPoint.get.x + x
      relativeY = startPoint.get.y + y
      pointGuide.foreach(_(Vector2D(startPoint.get.x + x,startPoint.get.y + y)).foreach(s => g.draw(s.transform(t))))
    }
  }
}