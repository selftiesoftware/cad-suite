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

import com.siigna.app.controller.pgsql_handler._


//siigna
import com.siigna._

object Open extends Module {

  //val fromDatabase = new LineShape(p1,p2, _)

 /*
  GetShapesInArea:
    Modtager x- og y-koordinat (Int), (default: 0,0)
    samt afstand fra centerpunkt i x- og y-retning, der skal søges
    og returnerer en sequence af: (shapeType1 (Int),shapeId1 (Int), shapeType2 (Int),shapeId2 (Int)...,...,...)
    def getShapesInArea (xCoordinate: Int, yCoordinate: Int, xDistance: Int, yDistance: Int) =
 */

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
        Siigna display "Loading drawings from the Siigna universe"
        Goto('End)
      }),
    'End   -> ((events : List[Event]) => {
      //connect to database and get all ShapeType and object IDs in it.
      val pgsqlShapes = new pgsqlGetShapesInArea()
      val lines = pgsqlShapes.getShapesInArea(-40000,-40000,80000,80000)

      //val getVectors = new pgsqlGetLine.getLine(43))

        lines.foreach{
          case i : Int => {
            if(i != 2)
              //get points based on ID
              println(i)
              //getLine: Modtager      shapeId og returnerer (PointId1, x1-, y1- og z1-koordinat (Int), PointId2, x2-, y2- og z2-koordinat (Int)).
              //get Vector data from each ID
              //println(getVectors.getLine(i))

          }
          //convert the coordinates to LineShapes

          //and add them to the model.

        }

    })
  )
}
