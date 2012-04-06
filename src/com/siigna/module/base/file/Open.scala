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

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
        Siigna display "Loading drawings from the Siigna universe"
        Goto('End)
      }),
    'End   -> ((events : List[Event]) => {
      //connect to database and get all ShapeType and object IDs in it.
  //    val pgsqlShapes = new pgsqlGetShapes()
  //    val shapes = pgsqlShapes.getDrawingFromId(1)
      //val shapes = pgsqlShapes.getShapes(0, 0, 0, 10000, 10000, 10000)
  //    Create(shapes)
      /*val getVectors = new pgsqlGetLine
      val startTime = System.currentTimeMillis()
      var lineNumbers = 0
      //val query = new pgsql_db_query
      //val lines = new pgsqlGetShapesInArea
      println("lines: "+lines)
        lines.foreach{
          case i : Int => {
            if(i != 2){
              lineNumbers = lineNumbers + 1
              //get Vector data from each ID
              val coords = getVectors.getLine(i)
              println(coords)
              //convert the coordinates to LineShapes
              shape = Some(LineShape(Vector2D(coords._2,coords._3),Vector2D(coords._6,coords._7)))
              Create(shape)
            }
          }
        }
      val endTime = System.currentTimeMillis()
      Siigna.display("loaded "+lineNumbers+" LineShapes in "+((endTime - startTime)/1000)+" seconds.")
      */
    })
  )
}
