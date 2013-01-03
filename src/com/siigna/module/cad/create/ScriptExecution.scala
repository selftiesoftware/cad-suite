/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.cad.create

import com.siigna._

/*class ScriptExecution extends Module {

  //get text from the editor
  val text = com.siigna.module.base.create.ScriptEditor.text
  
  //a hack to prevent the script from being executed twice.
  var executionComplete = false

  def stateMachine = Map(
    'StartCategory -> ((events : List[Event]) => {
      if(!executionComplete) {
        executionComplete = true
        Siigna display "running script"
        Goto('Execute)
      }
    }),
    
    //execute the script, activated if the run button is clicked.
    'Execute -> ((events : List[Event]) => {
      //TODO: make an execute function that evaluates loops, lists, functions etc. and parses it into a lis of shapes to be created.

      //EVALUATION (mock up)

      val line = "line(.*)".r
      val polyline = "polyline(.*)".r

      //TODO: add support for multiple lines of text

      text match {
        case line(x) => {
          if(x.contains(",")) { val c = x.split(",").toList
            var p1x = c(0).substring(1).toDouble
            var p1y = c(1).reverse.substring(1).reverse.toDouble
            var p2x = c(2).substring(1).toDouble
            var p2y = c(3).reverse.substring(1).reverse.toDouble

            CreateCategory(LineShape(Vector2D(p1x,p1y),Vector2D(p2x,p2y)))
          } else {
            Siigna display "found a line with wrong syntax: "+x
            Goto('StartCategory)
          }
          Goto('End)
        }
        //display error message if the script will not compile
        case _ =>
          Siigna display ("syntax error in script:" +text)
          Goto('StartCategory)
      }
      None
    }),
    'End -> ((events : List[Event]) => {
      //reset vars
      executionComplete = false
      Siigna display  "execution complete"
    })
  )
}
*/