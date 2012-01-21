/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna.app.controller.pgsql_handler._

//siigna
import com.siigna._

object Save extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  private var hasShape = false

  lazy val stateMap     = DirectedGraph  ('Start     -> 'KeyEscape -> 'End)

  def dbShape (shapeId: Int, point1Id: Int, point2Id: Int) = new PgsqlSaveShapeLine().postgresSaveShapeLine(_, _, _, _, _, _)

  //definition of the coordinates
  var x1 : Option[Int] = None
  var y1 : Option[Int] = None
  var z1 : Option[Int] = None
  var x2 : Option[Int] = None
  var y2 : Option[Int] = None
  var z2 : Option[Int] = None

  var selectedShape : Set[Shape] = Set()

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //display a message instructing to drag a selection around the lines that should e saved to the database
      interface display("select lines to save to database")
      events match {
        //if a message is returned from the select module, use the geometry that has been passed
        case Message(shapes : Set[Shape]) :: tail => {
          //save the incoming shapes in a var
          selectedShape = shapes
          //set a flag so the module knows if there was actually any data in the set
          if(!selectedShape.isEmpty)
            hasShape = true
        }
        case _ => {
          //if there is any data, proceed to 'End
          if(hasShape == true) {
           Goto('End)
          }
          //if not, offer the possibility to try ans select again.
          else {
            ForwardTo('Select)
          }
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      //return without a result if there are no lines
      if (hasShape == false) {
        interface display("found no lines")
        Thread.sleep(1000)
      }
      //advice the user if the selection was empty
      else if(hasShape == true && selectedShape.isEmpty) {
        interface display("found an empty set")
        Thread.sleep(1000)
      }
      //save the data
      else {
        val list = selectedShape.toList
        var p1 : Vector2D = Vector2D(0,0)
        val polylineShape = list.head

        println("first item: "+polylineShape)

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
      hasShape = false
      interface.clearDisplay()
    })
  )
}
