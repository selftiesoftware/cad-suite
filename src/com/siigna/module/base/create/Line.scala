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
import app.controller.Controller
import app.Siigna
import java.awt.Color

/**
 * A line module (draws one line-segment)
 */
class Line extends Module {

  private var coordinateX : Option[Double] = None   //text input for X values
  private var coordinateY : Option[Double] = None   //text input for Y values
  private var coordinateValue : String = ""  //input string for distances

  var guide : Guide = Guide((v : Vector2D) => {
    Array(LineShape(startPoint.get, v))
  })
  var startPoint: Option[Vector2D] = None

  // Save the X value, if any
  def x : Option[Double] = if (!coordinateX.isEmpty)
    coordinateX
  else if (coordinateValue.length > 0 && coordinateValue != "-")
    Some(java.lang.Double.parseDouble(coordinateValue))
  else if (coordinateX.isDefined)
    Some(coordinateX.get)
  else None

  // Save the Y value, if any
  def y : Option[Double] = if (coordinateY.isDefined)
    coordinateY
  else if (coordinateX.isDefined && coordinateValue.length > 0 && coordinateValue != "-")
    Some(java.lang.Double.parseDouble(coordinateValue))
  else if (coordinateY.isDefined)
    Some(coordinateY.get)
  else None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(v : Vector2D) :: tail => {
        if (startPoint.isEmpty){
          startPoint = Some(v)

          Start('Point,"com.siigna.module.base.create", guide)
        } else {

          val lShape = LineShape(startPoint.get,v)

          def setAttribute[T : Manifest](name:String, shape:Shape) = {
            Siigna.get(name) match {
              case s : Some[T] => shape.addAttribute(name, s.get)
              case None => shape// Option isn't set. Do nothing
            }
          }

          val line = setAttribute[Color]("Color",
            setAttribute[Double]("LineWeight", lShape)
          )
          Create(line)
          End
        }
      }
      //goto second coordinate if ENTER, COMMA, or TAB is pressed
      case End(KeyDown(Key.Enter | Key.Tab | (','), _)) :: tail => {
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

          //if a full set of coordinates are present, draw the shape.
          Create(LineShape(startPoint.get, Vector2D(coordinateX.get + startPoint.get.x,coordinateY.get+startPoint.get.y)))

          End
        }
      }
      //TODO: Backspace is inoperative...?
      case KeyDown(Key.Backspace, _) :: tail => {
        if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
        else if (coordinateX.isDefined) {
          coordinateValue = coordinateX.get.toString
          coordinateX     = None
        }
      }

      //if point returns a keyDown - that is not previously intercepted
      case End(KeyDown(code, _)) :: tail => {
        println(coordinateValue)
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

      //if point returns a mouseDown
      case End(m : MouseDown) :: tail => {
        println("Mouse button pressed - other than left..." + m)
        if (startPoint.isDefined) {
          guide = Guide((v : Vector2D) => {
            Array(LineShape(startPoint.get, v))
          })
          Start('Point,"com.siigna.module.base.create", guide)
        } else {
          Start('Point,"com.siigna.module.base.create", guide)
        }
      }
      case _ => {
      }
      //if points are in the process of being typed, then send the current value in a point guide.
      if(x.isDefined && !y.isDefined){
        guide = Guide((v : Vector2D) => {
          Array(LineShape(startPoint.get, Vector2D(x.get + startPoint.get.x,startPoint.get.y)))
        })
      }
      else if(x.isDefined && y.isDefined){
        guide = Guide((v : Vector2D) => {
          Array(LineShape(startPoint.get, Vector2D(x.get + startPoint.get.x,y.get + startPoint.get.y)))
        })
      }
      Start('Point,"com.siigna.module.base.create", guide)
    }
  )
}