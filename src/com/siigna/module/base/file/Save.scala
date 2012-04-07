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

/* 2010 (C) Copyright by Siigna, all rights reserved. */

import com.siigna.app.controller.pgsql_handler._

 /*
   a test module for Database connection:
     selects all (Poly)LineShapes in the model and saves them to the database.
     to test for a successful save operation:
     goto siigna.php
     in the field 'Direkte PostgreSQL-query':
     type:
        selectedShape * from shape
     or select * from shape_point
     or select * from shape_point_relation
     to see id shapes are saved.
 */

//siigna
import com.siigna._
import com.siigna.app.controller.pgsql_handler.pgsqlSave

object Save extends Module {

  def eventHandler = EventHandler(stateMap, stateMachine)

  private var hasShape  = false

  lazy val stateMap     = DirectedGraph  ('Start     -> 'KeyEscape -> 'End)

  //definition of the coordinates
  var x1 : Option[Int] = None
  var y1 : Option[Int] = None
  var z1 : Option[Int] = None
  var x2 : Option[Int] = None
  var y2 : Option[Int] = None
  var z2 : Option[Int] = None

  var selectedShape : Set[Shape] = Set()

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //display a message instructing to drag a selection around the lines that should e saved to the database

      if(!Model.isEmpty){

        Siigna.display("saving to database")

        Goto('End)
      }
      else {
        Siigna.display("no lines in drawing. Ending Save module")
        Goto('End)
      }

    }),
    'End   -> ((events : List[Event]) => {
      //proceed to save the data
      Siigna display "saving to Siigna Universe, please wait"
      //Gemmer her alle shapes i modellen i tegningen med id 5, da mappen, der gemmes er modellen (Model.seq)....
      pgsqlSave.mapOfShapesIntoDrawing(Model.seq,com.siigna.app.model.drawing.activeDrawing.drawingId.get)

      /*
      if(!Model.isEmpty) {
        //the result is a set that need to be converted:
        Model foreach (_ match {
          case l : PolylineShape => {
            l.shapes.foreach {
              case l : LineShape => {
                println("L: "+l)
                val p1 = l.p1
                val p2 = l.p2
                val x1 = p1.x.round.toInt
                val y1 = p1.y.round.toInt
                val z1 = 0
                val x2 = p2.x.round.toInt
                val y2 = p2.y.round.toInt
                val z2 = 0
                val pgsql = new PgsqlSaveShapeLine()
                val ids = pgsql.postgresSaveShapeLine(x1,y1,z1,x2,y2,z2)
                println(ids)
              }
              case _ =>
            }
          }
          case _ => println("incompatible shape")
        })

        x1 = Some(10)
        y1 = Some(50)
        z1 = Some(0)
        x2 = Some(200)
        y2 = Some(50)
        z2 = Some(0)
        //save the shape
        //dbShape(1,2,3) = (x1,y1,z1,x2,y2,z2)
      }

      //clear variables
      */
    })
  )
}
