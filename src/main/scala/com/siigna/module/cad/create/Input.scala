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
import module.Tooltip

/**
 * An input request module.
 */
class Input extends Module {

  //Information received from calling module
  var inputRequest: Option[InputRequest] = None
  var inputType: Option[Int] = None
  var guides: Seq[Guide] = Seq()
  var referencePoint: Option[Vector2D] = None
  var trackDoubleRequest: Boolean = false
  var movedAwayFromEntryPoint: Boolean = false
  var entryPoint: Option[Vector2D] = None

  def interpretMouseInput(p : Vector2D) : Option[ModuleEvent] = {
    if (inputType == Some(1) || inputType == Some(2) || inputType == Some(5) || inputType == Some(6) || inputType == Some(7)
      || inputType == Some(9) || inputType == Some(14) || inputType == Some(15) || inputType == Some(16))  {
      //Absolute values returned
      Some(End(p.transform(View.deviceTransformation)))
    } else if (inputType == Some(10)) {
      //Type is distance from start point, returned as double
      val startPointX = referencePoint.get.x
      val startPointY = referencePoint.get.y
      val distanceFromStartToMouse: Double = math.sqrt(( (startPointX-p.transform(View.deviceTransformation).x) * (startPointX-p.transform(View.deviceTransformation).x)) + ( (startPointY-p.transform(View.deviceTransformation).y) * (startPointY-p.transform(View.deviceTransformation).y)) )
      if (distanceFromStartToMouse != 0) {
        Some(End(distanceFromStartToMouse))
      } else None
    } else if (inputType == Some(999)) {
      //Relative values returned
      Some(End((p+referencePoint.get).transform(View.deviceTransformation)))
    } else if (inputType == Some(13)) {
      Some(End)
    } else None
  }

  val stateMap: StateMap = Map(
    'Start -> {
      case Start(_ , i: InputRequest) :: tail => {
        inputRequest = Some(i)
        inputType = Some(i.inputType)
        guides = i.guides
        referencePoint = i.referencePoint
        //Snap to a) shapes in the making, b) a snapGuide of c) nothing as required
        var snapGuide: Boolean = false
        guides.foreach(_ match {
          case Vector2DGuide(guide) => {
            if (snapGuide == false && inputType != Some(5) && inputType != Some(8) && inputType != Some(15) && inputType != Some(16) && inputType != Some(18)) {
              val snapFunction = () => guide(mousePosition)
              eventParser.snapTo(snapFunction)
            }
          }
          case _ => // No known guide
        } )

        //Turn off track,if required
        if(inputType == Some(2) || inputType == Some(8) || inputType == Some(15)) Siigna("track") = false
        'ReceiveUserInput
      }

      case x => {
        println("Input was started, but no input request was sent, or the match on input request failed. Input ends. Event-stream: " + x)
        if (!Siigna.isTrackEnabled) Siigna("track") = true
        End
      }
    },

  'ReceiveUserInput -> {
    //Input from mouse-actions:

    //Left mouse button down
    case MouseDown(p,MouseButtonLeft,_) :: tail => interpretMouseInput(p).getOrElse(None)
    case End(MouseDown(p, MouseButtonLeft, _)) :: tail => interpretMouseInput(p).getOrElse(None)

    //Left mouse button up:
    case MouseUp(p,MouseButtonLeft,modifier)::tail => {
      if (inputType == Some(8) || inputType == Some(18)) {
        if (!Siigna.isTrackEnabled) Siigna("track") = true
        End(p.transform(View.deviceTransformation))
    }}

    //Right mouse button down
    case MouseDown(p,MouseButtonRight,modifier)::tail => {
      //Standard: the mouseDown action is returned
      if (!Siigna.isTrackEnabled) Siigna("track") = true
      End(MouseDown(p.transform(View.deviceTransformation),MouseButtonRight,modifier))
    }

    //Input from keyboard:
      //AngleGizmo
    case KeyDown(Key.shift, _) :: tail => {
      if (inputType == Some(1) || inputType == Some(2) || inputType == Some(3) || inputType == Some(4) || inputType == Some(5)
        || inputType == Some(6) || inputType == Some(7) || inputType == Some(8) || inputType == Some(9) || inputType == Some(10)
        || inputType == Some(11) || inputType == Some(12) || inputType == Some(16) || inputType == Some(18)) {
        //Angle gizmo based on track point (Track point is sent to Angle Gizmo - not referencePoint (if there is one)
        if (Track.isTracking && Track.pointOne.get.distanceTo(mousePosition.transform(View.deviceTransformation)) < Siigna.selectionDistance) {
          Start('cad,"create.AngleGizmo",InputRequest(inputType.get,Track.pointOne,guides:_*))
        //Angle gizmo based on reference point (The angle gizmo gets a track point, if one is active. If none is active, the reference point is sent.
        //If there is neither track- or reference point, Angle gizmo does not start (since it wouldn't know where it's centre should be).
        } else if ((inputType == Some(5) || inputType == Some(7) || inputType == Some(10)) && !referencePoint.isEmpty) {
          Start('cad,"create.AngleGizmo",inputRequest.get)
        }
      }
    }


    //Most key-inputs are not handled directly in Input, but sorted and forwarded to key-input modules.
    //Some are, however - eg. enter, escape and backspace.
    case KeyDown(key,modifier) :: tail => {
      if (trackDoubleRequest) trackDoubleRequest = false
      //ENTER: Is returned to the asking module as a key-down event:
      if (key == Key.enter) {
        Siigna("track") = true
        End(KeyDown(key,modifier))
      }
      //ESCAPE: Is returned to the asking module as a key-down event:
      else if (key == Key.escape) {
        Siigna("track") = true
        End(KeyDown(key,modifier))
      }
      //BACKSPACE with no modifiers: Is returned to the asking module as a key-down event:
      else if (key == Key.backspace) {
        Siigna("track") = true
        End(KeyDown(key,modifier))
      }
      //Input types where track-offset is activated: Vector2D-guides are transformed to DoubleGuides:
      //Guides only start when the mouse has been moved away from the point where it entered into input - so entry og x,y isn't interpreted as a distance on a guide...
      else if (movedAwayFromEntryPoint == true && (inputType == Some(4) || inputType == Some(5) || inputType == Some(6) || inputType == Some(7) || inputType == Some(9)
        || inputType == Some(16) || inputType == Some(18)) && Track.isTracking) {
        val guidesNew = guides.collect({
          case Vector2DGuide(guide) => {
            DoubleGuide((d : Double) => {
              guide(Track.getPointFromDistance(d).get)
            })
          }
          case Vector2DGuideKeys(guide) => {
            DoubleGuide((d : Double) => {
              guide(Track.getPointFromDistance(d).get - Track.pointOne.get)
            })
          }
        })
        val newInputRequest = InputRequest(7,referencePoint,guidesNew:_*)
        println("DER")
        trackDoubleRequest = true
        Start('cad,"create.InputSingleValueByKey",newInputRequest)
      // Input types accepting Vector2D by keys:
      } else if(inputType == Some(3) || inputType == Some(5)
        || ((inputType == Some(4) || inputType == Some(6) || inputType == Some(7) || inputType == Some(16)) && (!Track.isTracking || movedAwayFromEntryPoint == false))) {
        Start('cad,"create.InputDualValuesByKey",inputRequest.get)
      }
       else if (inputType == Some(9) || inputType == Some(10) || inputType == Some(11) || inputType == Some(13) || inputType == Some(15)) {
      // Input types accepting a double as input:
        Start('cad,"create.InputSingleValueByKey",inputRequest.get)
      } else if(inputType == Some(12)) {
      // Input types accepting a string as input:
        Start('cad,"create.InputTextByKey",inputRequest.get)
      } else if(inputType == Some(14)) {
        // Input types accepting a character as input:
        Start('cad,"create.InputCharacterByKey",inputRequest.get)
      }
    }

    //Input received from other modules (eg. InputSingleValue/DualValue/Text/CharacterByKey, AngleGizmo):

    //Vector2D: (Standard: The received Vector2D is returned, un-transformed)
    case End(p : Vector2D) :: tail => {
      //if (drawGuideInInputModule == false) drawGuideInInputModule = true
      if (inputType == Some(5) || inputType == Some(7) && !referencePoint.isEmpty) {
        Siigna("track") = true
        End(referencePoint.get + p)
      } else if (inputType == Some(16)){
        End(MouseDown(p,MouseButtonLeft,ModifierKeys(false,false,false)))
      } else {
        Siigna("track") = true
        End(p)
      }
    }

    //Double:
    case End(s : Double) :: tail => {
      if (trackDoubleRequest && (inputType == Some(4) || inputType == Some(5) || inputType == Some(6)|| inputType == Some(7) || inputType == Some(9))) {
        trackDoubleRequest = false
        Siigna("track") = true
        End(Track.getPointFromDistance(s).get)
      } else if (inputType == Some(16)) {
        trackDoubleRequest = false
        Siigna("track") = true
        End(MouseDown(Track.getPointFromDistance(s).get - Track.pointOne.get,MouseButtonNone,ModifierKeys(false,false,false)))
      } else if (inputType == Some(9) || inputType == Some(10) || inputType == Some(11) || inputType == Some(13) || inputType == Some(15)) {
        Siigna("track") = true
        End(s)
      }
    }

    //String:
    case End(s : String) :: tail => {
      Siigna("track") = true
      End(s)
    }

    //Character or special key:
    case End(KeyDown(code: Int,modifier: ModifierKeys)) :: tail => {
      End(KeyDown(code: Int,modifier: ModifierKeys))
    }

    case MouseMove(p,_,_) :: tail => {
      if (movedAwayFromEntryPoint == false) {
        if (entryPoint.isEmpty) entryPoint = Some(p)
        else if (p.distanceTo(entryPoint.get) > Siigna.selectionDistance) movedAwayFromEntryPoint = true
      }
    }

    case MouseDrag(p,_,_) :: tail => {
      if (movedAwayFromEntryPoint == false) {
        if (entryPoint.isEmpty) entryPoint = Some(p)
        else if (p.distanceTo(entryPoint.get) > Siigna.selectionDistance) movedAwayFromEntryPoint = true
      }
    }

    case _ => {
      Tooltip.refresh()
    }
  })


  //draw the guide - but only if no points are being entered with keys, in which case the input modules are drawing.
  override def paint(g : Graphics, t : TransformationMatrix) {
    if (!isForwarding) {
      guides.foreach(_ match {
        case Vector2DGuide(guide) => {
          guide(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t)))
        }
        case _ =>
      } )
    }
  }
}

trait Guide {
  def guide : ((_) => Traversable[Shape])
}

//The input request to be sent to the input module. Contains the information, the module needs to pass to the input module. Consists of:
// 1) inputType: An integer, which tells the input-module what input the asking module needs. For description of types, see below.
// 2) referencePoint: A reference point, if it is required. If no reference point is required, write None - as it is an option[Vector2d]
// 3) guide(s): One or more guides, used for drawing the shapes dynamically based on the user-input. Seperate guides by , if there are more than one.
//    If there is no guide, don't write anything here.

case class InputRequest(inputType: Int, referencePoint: Option[Vector2D], guides : Guide*)

//The most common input-types' basic features (for complete features and advanced input types, ask Niels for reference sheet :-) ):

// inputType Left Mouse Down                        Keys                                Track-offset default	 Reference point(s)  AngleGizmo default
// 6		     Vector2D, absolute                     Vector2D, absolute                  On	                   None                On
// 7         Vector2D, absolute                     Vector2D, added to reference point  On                     One                 On
// 10		     Double, distance from reference point  Double                              Off	                   One                 On
// 11        None                                   Double                              Off                    None                On
// 12        None                                   String                              Off                    None                On


case class DoubleGuide(guide : Double => Traversable[Shape]) extends Guide
case class Vector2DGuide(guide : Vector2D => Traversable[Shape]) extends Guide
case class TextGuide(guide : String => Traversable[Shape]) extends Guide
//An extra guide: To be able to draw a guide when inputting Vector2D by keys, but not when inputting by mouse
case class Vector2DGuideKeys(guide : Vector2D => Traversable[Shape]) extends Guide
