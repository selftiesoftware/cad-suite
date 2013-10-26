



/*
* Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
* to Share — to copy, distribute and transmit the work,
* to Remix — to adapt the work
*
* Under the following conditions:
* Attribution — You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
* Noncommercial — You may not use this work for commercial purposes.
* Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
*/

package com.siigna.module.cad.edit

import com.siigna._
import app.Siigna
import com.siigna.module.cad.create._
import module.{Tooltip, ModuleInit}

class Rotate extends Module {

  private var centerPoint : Option[Vector2D] = None
  private var endVector : Option[Vector2D] = None
  var startVectorSet: Boolean = false
  var firstPoint: Boolean = true
  var mouseReleased: Boolean = true
  var mouseDownPoint: Option[Vector2D] = None
  val origin = Drawing.selection.transformation

  private var startVector : Option[Vector2D] = Some(Vector2D(0,0))
  var transformation : Option[TransformationMatrix] = None

  val doubleGuide = DoubleGuide((d: Double) => {
    Drawing.selection.transformation = origin
    val t : TransformationMatrix = TransformationMatrix( ).rotate(-d, centerPoint.get)
    Drawing.selection.transform(t).shapes.values
  })

  val vector2DGuideMouseUp = Vector2DGuide((v : Vector2D) => {
    Drawing.selection.transformation = origin
    val rotateAngle = -((mouseDownPoint.get - centerPoint.get).angle - (v - centerPoint.get).angle)
    val t : TransformationMatrix =
      TransformationMatrix( ).rotate(rotateAngle, centerPoint.get)
    Drawing.selection.transform(t).shapes.values
  })

  val vector2DGuideRotation = Vector2DGuide((v: Vector2D) => {
    Drawing.selection.transformation = origin
    //Angle of line from reference point to mouse position:
    val d : Double = (-((mousePosition.transform(View.deviceTransformation) - centerPoint.get).angle - (startVector.get - centerPoint.get).angle))
    val t : TransformationMatrix = TransformationMatrix( ).rotate(-d, centerPoint.get)
    Drawing.selection.transform(t).shapes.values
  })

  val stateMap: StateMap = Map(

    'Start -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail =>

      //If none of the above happens, module should go on like this:
      case _ => {
        //If nothing is selected, start select
        if (Drawing.selection.isEmpty) {
          Siigna display "Select objects to rotate"
          Tooltip.blockUpdate(3500)
          Start('cad, "Selection")
        } else {
          //Update tooltip
          Tooltip.updateTooltip(List("Rotate tool active"))
          Siigna display "set centre point for rotation"
          Tooltip.blockUpdate(3500)
          //Go to next state
          'StateOne
        }
      }
    },

    'StateOne -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input
      case End(v : Vector2D) :: tail => {
        if(centerPoint.isEmpty) {
          centerPoint = Some(v)
          Siigna display "click to set rotation start point, or type a rotation angle"
          Tooltip.blockUpdate(3500)
          //Request mouse down - it might be a rotation start point, or the start of a rotation angle:
          Start('cad, "create.Input", InputRequest(15,None,doubleGuide))
        } else if (centerPoint.isDefined && firstPoint == true) {
          firstPoint = false
          mouseDownPoint = Some(v)
          //Find out where the mousebutton is released - if it's a start point or an angle that's defined:
          Start('cad, "create.Input", InputRequest(8,None,vector2DGuideMouseUp))
          //Where the mouse up is:
        } else if (mouseReleased == true) {
          mouseReleased = false
          //If an angle has been defined, the rotation is complete:
          if (v!= mouseDownPoint.get) {
            Drawing.deselect()
            End
          } else {
            //Else its the start vector:
            startVector = Some(v)
            startVectorSet = true
            Siigna display "click to finish rotation, or type a rotation angle"
            Tooltip.blockUpdate(3500)
            val inputRequest = InputRequest(15,None,vector2DGuideRotation,doubleGuide)
            Start('cad, "create.Input", inputRequest)
            //If a rotation-vector is set, do the rotation!
          }
        } else if(startVectorSet == true && centerPoint.isDefined) {
          Drawing.deselect()
          End
        }
      }

      //if Point returns a typed angle:
      case End(a : Double) :: tail => {
        Drawing.selection.transformation = origin
        val t : TransformationMatrix = {
          if (centerPoint.isDefined) {
            TransformationMatrix(Vector2D(0,0), 1).rotate(-a, centerPoint.get)
          } else TransformationMatrix()
        }
        //val t = transformation.get.rotate(rotation,centerPoint.get)
        Drawing.selection.transform(t)
        Drawing.deselect()
        End
      }

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail => //Does nothing

      //Request input
      case _ => Start('cad, "create.Input", InputRequest(6,None))
    }
  )
}