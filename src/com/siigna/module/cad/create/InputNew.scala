package com.siigna.module.cad.create

import com.siigna._

/**
 * Created by IntelliJ IDEA.
 * User: Niels Egholm
 * Date: 19-03-13
 * Time: 17:09
 * To change this template use File | Settings | File Templates.
 */

class InputNew extends Module {

  //Information received from calling module
  var inputRequest: Option[InputRequestNew] = None
  var inputType: Option[Int] = None
  var guides: Seq[Guide] = Seq()
  var referencePoint: Option[Vector2D] = None

  //State of behaviour of input module
  var drawGuideInInputModule: Boolean = true
  
  val stateMap: StateMap = Map(
    'Start -> {
      case Start(_ , i: InputRequestNew) :: tail => {
        inputRequest = Some(i)
        inputType = Some(i.inputType)
        guides = i.guides
        referencePoint = i.referencePoint
        'ReceiveUserInput
      }
      case _ => {
        End
      }
    },

  'ReceiveUserInput -> {
    //Input from mouse-actions:

    //Left mouse button down (Standard: the clicked point is returned, transformed to view):
    case MouseDown(p,MouseButtonLeft,modifier)::tail => {
      if (inputType == Some(1) || inputType == Some(2) || inputType == Some(5) || inputType == Some(6) || inputType == Some(7))  {
        End(p.transform(View.deviceTransformation))
      } else if (inputType == Some(7)) {
        End((p+referencePoint.get).transform(View.deviceTransformation))
      }
    }

    //Input from keyboard:

    //Most key-inputs are not handled directly in Input, but sorted and forwarded to key-input modules.
    //Some are, however - eg. escape and backspace.
    case KeyDown(key,modifier) :: tail => {
      //ESCAPE: Is returned to the asking module as a key-down event:
      if (key == Key.escape) End(KeyDown(key,modifier))
      //BACKSPACE with no modifiers: Is returned to the asking module as a key-down event:
      else if (key == Key.backspace) End(KeyDown(key,modifier))
      //Any other keys keys, if Vector2D by keys is accepted as input:
      else if(inputType == Some(3) || inputType == Some(4) || inputType == Some(5) || inputType == Some(6) || inputType == Some(7)) {
        //if (drawGuideInInputModule == true) drawGuideInInputModule = false
        Start('cad,"create.InputValuesByKey",inputRequest.get)
      }
    }

    //Input received from other modules (eg. Input OneValue, InputTwoValues, InputText, AngleGizmo):

    //Vector2D: (Standard: The received Vector2D is returned, un-transformed)
    case End(p : Vector2D) :: tail => {
      //if (drawGuideInInputModule == false) drawGuideInInputModule = true
      if (inputType == Some(7) && !referencePoint.isEmpty) {
        End(referencePoint.get + p)
      } else {
        End(p)
      }
    }

    case _ => {
    }
  })


  //draw the guide - but only if no points are being entered with keys, in which case the input modules are drawing.
  override def paint(g : Graphics, t : TransformationMatrix) {

    if ( drawGuideInInputModule == true) {

      guides.foreach(_ match {
        case Vector2DGuideNew(guide) => {
          guide(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t)))
        }
        case _ => // No known guide
      } )
    }
  }
}

trait Guide {
  def guide : _ => Traversable[Shape]
}

//The input request to be sent to the input module. Contains the information, the module needs to pass to the input module. Consists of:
// 1) inputType: An integer, which tells the input-module what input the asking module needs. For description of types, see below.
// 2) referencePoint: A reference point, if it is required. If no reference point is required, write None - as it is an option[Vector2d]
// 3) guide(s): One or more guides, used for drawing the shapes dynamically based on the user-input. Seperate guides by , if there are more than one.
//    If there is no guide, don't write anything here.

case class InputRequestNew(inputType: Int, referencePoint: Option[Vector2D], guides : Guide*)

// inputType	Return	  Input method              Track-offset default	AngleGizmo default	Absolute coordinates	Relative coordinates	Reference point(s)
// 1		      Vector2D	Left mouse down           Off	                  Off	                Yes	                  No	                  None
// 2		      Vector2D	Left mouse down           On	                  On	                Yes	                  No	                  None
// 3		      Vector2D	Keys                      Off	                  Off	                Yes	                  No	                  None
// 4		      Vector2D	Keys                      On	                  On	                Yes	                  No	                  None
// 5		      Vector2D	Keys or left mouse down   Off	                  Off	                Yes	                  No	                  None
// 6		      Vector2D	Keys or left mouse down   On	                  On	                Yes	                  No	                  None
// 7		      Vector2D	Keys or left mouse down   On	                  On	                No	                  Yes	                  One

// 10		Double	Off	Off	N/A	N/A	None	Keys


case class DoubleGuideNew(guide : Double => Traversable[Shape]) extends Guide
case class Vector2DGuideNew(guide : Vector2D => Traversable[Shape]) extends Guide
case class TextGuideNew(guide : String => Traversable[Shape]) extends Guide
case class DoubleMessageGuideNew(guide : Double => Traversable[Shape]) extends Guide
case class Vector2DMessageGuideNew(guide : Vector2D => Traversable[Shape]) extends Guide
case class TextMessageGuideNew(guide : String => Traversable[Shape]) extends Guide