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

//import statements
package com.siigna.module.base.create

import com.siigna._

// definition of the object:
object Template extends Module {


/**
 * A template module for use as to create new modules.
 * Create a line where the mouse is clicked and writes the text "Hello Siigna World" dynamically while moving the mouse,
 * and in a display after the mouse is clicked.
 * by Ole Egholm
 */


  //********* VARIABLES DECLARATION ***********


  val eventHandler = EventHandler(stateMap, stateMachine)

  private var point : Option[Vector2D] = None

  private var shape : Option[LineShape] = None

  //the text string to be created
  private var text = "Creating a line in the Siigna World!"

  def stateMap = DirectedGraph(
    'Start    ->   'KeyEscape ->    'End
  )

  def stateMachine = Map(

    //********* START STATE ***********


    //'Start is always the first state, and always present in a module.
    'Start -> ((events : List[Event]) => {
    //declaration of a 'Point Guide, that will send the text string to 'Point
    // to draw it dynamically while looking for a MouseDown (waiting for the user to click) in 'Point.
    def getPointGuide = (p : Vector2D) =>
      TextShape(text,p, 10 * Siigna.paperScale, Attributes())

      //Log.level += Log.DEBUG + Log.SUCCESS
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => Goto('End)

        //if point returns a Message with a point, goto the 'End state to draw a line and end the module.
        case Message(p : Vector2D) :: tail => {
          point = Some(p)
          Goto('End)
        }
        // if everything but a right mouse click is registered, forward to the point module to look for a point.
        case _ => {
          //send the Point Guide ahead before Forwarding to 'Point.
          Controller ! Message(PointGuide(getPointGuide))
          ForwardTo('Point, false)
        }
      }
    }),

    //********* MORE STATES COULD BE INSERTED HERE ***********



    //********* END STATE ***********

    //As defined in the stateMap, 'Start will go to 'SetPoint once 'Point returns with a 'Message.:
    'End -> ((events : List[Event]) => {
      //match on incoming events:
      events match {
        // exit strategy
        case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)
        //write the text message if 'Points returns a Message with a 'Point (the point is returned as (p).
        case Message(p : Vector2D) :: tail => {

          //CREATE THE LINE:
          shape = Some(LineShape(point.get + Vector2D(0,-50), point.get + Vector2D(0, 50)))
          Create(shape)
        }

        // match on everything else
        case _ =>
      }
    })
  )
  //override def paint(g: Graphics, t : TransformationMatrix){
  //g draw xxxxxxx.transform(t)
  //}
}