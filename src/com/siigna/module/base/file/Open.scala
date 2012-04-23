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

package com.siigna.module.base.file

import com.siigna.app.model.drawing.activeDrawing._

//TODO: update the way IDs are generated, so that all shapes in a saved model area assigned a global coordinate in the Siigna Universe.
//Example: one model with two shapes: ID=000130000000, x,y, ID=000230000000, x,y where 0001 and 0002 is a running number and 30000000 the position.


//siigna
import com.siigna._

object Open extends Module {

  //DELETE DATABASE: delete from shape where point_id > 0
  //delete from shape where shape_id > 0
  //val fromDatabase = new LineShape(p1,p2, _)

 /*
  GetShapesInArea:
    Modtager x- og y-koordinat (Int), (default: 0,0)
    samt afstand fra centerpunkt i x- og y-retning, der skal søges
    og returnerer en sequence af: (shapeType1 (Int),shapeId1 (Int), shapeType2 (Int),shapeId2 (Int)...,...,...)
    def getShapesInArea (xCoordinate: Int, yCoordinate: Int, xDistance: Int, yDistance: Int) =
 */

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  private var shape : Option[Shape] = None

  var text     = ""

  lazy val stateMap     = DirectedGraph(

    'Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      Siigna display "type ID for the drawing you wish to load"
        Goto('TextInput)
      }),
    'TextInput -> ((events : List[Event]) => {
      events match {
        case KeyDown(Key.Backspace, _) :: tail => {
            if (text.length != 0) text = text.substring(0, text.length - 1)
            else Goto('End)
        }
        case KeyDown(Key.Enter, _) :: tail => Goto('End)
        case KeyDown(Key.Esc, _) :: tail => {
          text = ""
          Goto('End)
        }
        case KeyDown(key, _) :: tail => {
          text += key.toChar.toString.toLowerCase
          Siigna display text
        }
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ =>
      }
      None
    }),


    'End   -> ((events : List[Event]) => {

      Siigna display "loading file... please wait"
      println("Beder om ny shapeid")
      GetNewShapeId(com.siigna.app.controller.AppletParameters.getClient)

      //connect to database and get all ShapeType and object IDs in it.

      //Try to get drawing name - if unable, the drawing doesn't exist:
      /*val name = pgsqlGet.drawingNameFromId(text.toInt)
      if (name.isDefined) {
        //tell Siigna that the drawing with the given ID and name is now the active drawing
        com.siigna.app.model.drawing.activeDrawing.setActiveDrawingId(text.toInt)
        com.siigna.app.model.drawing.activeDrawing.setActiveDrawingName(name.get)
        //then load the contents of this drawing
        val shapes: Map[Int,ImmutableShape] = pgsqlGet.allShapesInDrawingFromDrawingIdWithDatabaseId(text.toInt)
        //If there are shapes in the drawing (it could be a clean sheet):
        if (shapes.size > 0 ) {Create(shapes)} else {println("Drawing is empty.")}
        //Set this drawing to "last active drawing for user" så den åbnes ved næste besøg...
        com.siigna.app.controller.pgsql_handler.pgsqlUpdate.lastActiveDrawingIdIntoContributorData(contributorId.get,drawingId.get)
      } else {
        println ("The drawing doesn't exist.")
      }
      */

      //reset the vars
      text = ""

    })
  )
}
