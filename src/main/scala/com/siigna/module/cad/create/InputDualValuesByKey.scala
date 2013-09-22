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

package com.siigna.module.cad.create

import com.siigna._
import com.siigna.app.Siigna

/**
 * Created by IntelliJ IDEA.
 * User: Niels Egholm
 * Date: 19-03-13
 * Time: 18:08
 * To change this template use File | Settings | File Templates.
 */

class InputDualValuesByKey extends Module {

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
      //If there is an entered y-value:
      var y: Option[String] = None
      if (!xCoord.isEmpty && value != "-") {
        if ((((java.lang.Double.parseDouble(value))*100)%1) != 0) {
          y = Some(coordinateValue.substring(0, coordinateValue.length-1))
          coordinateValue = y.get
        } else y = Some(value)
      }
      else if (!xCoord.isEmpty && value == "-") y = Some(value)

      if (!x.isEmpty && y.isEmpty) ("point (X: " + x.get)
      else if (!y.isEmpty || y.get.length() == 0) ("point (X: "+x.get+", Y: " + y.get)
      //Here must be something - at least a string eith a space -
      // as long as an empty string makes the whole thing go down...
      else "point (X: "
      //If the coord string is empty(y-coord deleted, or not entered yet:)
    } else if (coordinateValue.length() == 0) {
      val x: Option[String] = Some(if (!xCoord.isEmpty) {
        if (xCoord.get % 1 == 0) xCoord.get.toInt.toString
        else xCoord.get.toString
      } else value)
      var y: Option[String] = None
      if (!yCoord.isEmpty) {
        if (yCoord.get % 1 == 0) y = Some(yCoord.get.toInt.toString)
        else y = Some(yCoord.get.toString)
      }
      if (!x.isEmpty && x.get.length() > 0) {
        if (y.isEmpty) ("point (X: " + x.get + ", Y:")
        else ("point (X: " + x.get + ", Y: " + y.get + ")")
      }
      else ("point (X: " + x.get)
    }
    //Also here - at least a space
    else " "
  }

  //Information received from calling module
  var inputRequest: Option[InputRequest] = None
  var inputType: Option[Int] = None
  var guides: Seq[Guide] = Seq()
  var referencePoint: Option[Vector2D] = None

  var vector2DGuide: Option[Vector2DGuide] = None
  var doubleGuide: Option[DoubleGuide] = None
  var textGuide: Option[TextGuide] = None
  var referenceDouble: Option[Double] = None

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
      case MouseDown(p,MouseButtonRight,modifier) :: tail => End(MouseDown(p,MouseButtonRight,modifier))

      //Read numbers and minus, "," and enter as first entry, after drawing of guide, if a guide is provided:
      case Start(_ ,i: InputRequest) :: KeyDown(code, _) :: tail => {
          inputRequest = Some(i)
          inputType = Some(i.inputType)
          guides = i.guides
          referencePoint = i.referencePoint
          if(referencePoint.isEmpty) referencePoint = Some(Vector2D(0,0))
          //save the already typed key:
          if (code.toChar.isDigit) coordinateValue += code.toChar
          if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
          //if a comma or enter is the first thing entered, it is interpreted as zero:
          if ((code.toChar.toString == "," || code == Key.enter) && coordinateValue.length() == 0) coordinateX = Some(0.0)
          Siigna display message(coordinateValue, coordinateX, coordinateY)
          'ReceiveUserInput
        }
        case _ => {
          End
        }
      },

    'ReceiveUserInput -> {

      //exit mechanisms
      case KeyDown(Key.escape,modifier) :: tail => End
      case MouseDown(p,MouseButtonRight,modifier) :: tail => End

      //Read numbers and minus, "," and enter as first entry if no guide is provided:
      case Start(_,_) :: KeyDown(code, _) :: tail => {
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        //if a comma or enter is the first thing entered, it is interpreted as zero:
        if ((code.toChar.toString == "," || code == Key.enter) && coordinateValue.length() == 0) coordinateX = Some(0.0)
        Siigna display message(coordinateValue, coordinateX, coordinateY)
      }

      //goto second coordinate if ENTER, COMMA, or TAB is pressed
      case KeyDown(Key.Enter | Key.Tab | (','), _) :: tail => {
        //when ENTER, "," or TAB is pressed, and a value is set, this value is passed as the first coordinate relative to 0,0
        if (coordinateX.isEmpty) {
          if (coordinateValue.length > 0) {
            if (coordinateValue == "-") coordinateValue = "0"
            coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))
            if (((coordinateX.get * 100) % 1) != 0) coordinateX = Some((math.floor(coordinateX.get * 100))/100)
            else if (((coordinateX.get * 10) % 1) == 0) coordinateX = Some((math.floor(coordinateX.get * 10))/10)
            coordinateValue = ""
          } else coordinateX = Some(0.0)
          Siigna display message(coordinateValue, coordinateX, coordinateY)
        }
        //If X is already entered, it is interpreted as Y, and the module ends:
        else if (coordinateY.isEmpty) {
          if (coordinateValue.length > 0) {
            if (coordinateValue == "-") coordinateValue = "0"
            coordinateY = Some(java.lang.Double.parseDouble(coordinateValue))
            if (((coordinateY.get * 100) % 1) == 0) coordinateY = Some((math.floor(coordinateY.get * 100))/100)
            else if (((coordinateY.get * 10) % 1) == 0) coordinateY = Some((math.floor(coordinateY.get * 10))/10)
            coordinateValue = ""
          } else coordinateY = Some(0.0)
          Siigna display message(coordinateValue, coordinateX, coordinateY)
          //if a full set of coordinates are present, return them to the calling module.
          End(Vector2D(x,y))
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
          //If there is no active string, and also no active coordinate, the module ends:
        } else if (coordinateValue.length == 0 && coordinateX.isEmpty) {
          End("no point returned")
        }


      }
      //if point returns a keyDown - that is not previously intercepted
      case KeyDown(code, modifier) :: tail => {
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


      case x => {

        if (coordinateX.isEmpty && coordinateValue.length() == 0) End
      }
    })

  //draw the guide:
  override def paint(g : Graphics, t : TransformationMatrix) {

    guides.foreach(_ match {
      case Vector2DGuide(guide) => {
        guide(Vector2D(referencePoint.get.x + x,referencePoint.get.y + y)).foreach(s => g.draw(s.transform(t)))
      }
      //An extra guide: A Hack, to be able to draw a guide when inputting Vector2D by keys, but not when inputting by mouse
      case Vector2DGuideKeys(guide) => {
        guide(Vector2D(referencePoint.get.x + x,referencePoint.get.y + y)).foreach(s => g.draw(s.transform(t)))
      }
      case _ => // No known guide
    })
  }

}
