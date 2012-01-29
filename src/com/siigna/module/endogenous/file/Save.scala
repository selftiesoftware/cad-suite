package com.siigna.module.endogenous.file

/* 2010 (C) Copyright by Siigna, all rights reserved. */

/*import com.siigna.app.controller.pgsql_handler._



//siigna
import com.siigna._

object Save extends Module {

  def eventHandler = EventHandler(stateMap, stateMachine)

  private var hasShape  = false

  lazy val stateMap     = DirectedGraph  ('Start     -> 'KeyEscape -> 'End)

  def dbShape (shapeId: Int, point1Id: Int, point2Id: Int) = new PgsqlSaveShapeLine().postgresSaveShapeLine(_, _, _, _, _, _)

 // def getPointFromShape(shape : Shape) : List[Vector2D] = shape match {
 //   case p : PolylineShape => p.shapes.foldLeft(List[Vector2D]()) (list, shape)
 // }

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
      Siigna.display("select lines to save to database")



    //var pointPlId : Seq[Int] = Seq()
    //pointPlId = pointPlId :+ 7 :+ 8 :+ 0 :+ 89 :+ 86 :+ 0 :+ 2 :+ 3 :+ 0 :+ 7 :+ 6 :+ 0 :+ 2 :+ 4 :+ 0 :+ 33 :+ 1 :+ 0
    //val (shapeId1: Int, pointPlIdReturn) = new pgsqlSaveShapePolyline().postgresSaveShapePolyline(pointPlId)
    //println(shapeId1)
    //println(pointPlIdReturn)

      //test:
      //siigna.php
      //selectedShape * from shape
      //select * from shape_point
      //select * from shape_point_relation

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
            ForwardTo('Selection)
          }
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      //return without a result if there are no lines
      if (hasShape == false) {
        Siigna.display("found no lines")
        Thread.sleep(1000)
      }
      //advice the user if the selection was empty
      else if(hasShape == true && selectedShape.isEmpty) {
        Siigna.display("found an empty set")
        Thread.sleep(1000)
      }
      //save the data
      else {
        //the result is a set that need to be converted:
        selectedShape foreach (_ match {
          case l : PolylineShape => {
            println(l)
            //println(l.shapes.foldLeft(List[Vector2D]()) ((list, shape) => list ++ getPointFromShape(shape)))
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
      hasShape = false
    })
  )
}
*/