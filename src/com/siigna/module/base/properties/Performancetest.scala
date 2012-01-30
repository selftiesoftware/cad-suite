package com.siigna.module.base.properties

import java.awt.Color._

import com.siigna._

/**
 * A performance test module which creates 1000*1000 lines.
 */
object Performancetest extends Module {

  var shapes = scala.collection.immutable.Vector[Shape]()
  val eventHandler = EventHandler(stateMap, stateMachine)

  val limit = 1000

  lazy val stateMap = DirectedGraph('Intermezzo -> 'KeyDown -> 'End)

  lazy val stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      var i = 0
      val startSeconds = System.currentTimeMillis()

      do  {
          val pointA = Vector2D(i, 0)
          val pointB = Vector2D(i + 10, limit >> 1)

          val pointC = Vector2D(0, i)
          val pointD = Vector2D(limit >> 1, i + 10)

          //val vertShape = LineShape(pointA, pointB)
          val vertShape = LineShape(pointA, pointB)
          //val horizShape = LineShape(pointC, pointD)
          val horizShape = LineShape(pointC, pointD)

          i += 1

          shapes = shapes :+ vertShape :+ horizShape
      } while (i <= limit)

      val endSeconds = System.currentTimeMillis()
      Log("Time to draw "+ limit * limit +" lines:" + (endSeconds - startSeconds))
      Goto('Intermezzo)
    }),
    'Intermezzo -> ((events : List[Event]) => None),

    'End -> ((events : List[Event]) => {
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    g setColor black
    g.draw(shapes.map(_.transform(t)))
  }
}