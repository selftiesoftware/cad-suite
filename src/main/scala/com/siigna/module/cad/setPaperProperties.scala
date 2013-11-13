package com.siigna.module.cad

import com.siigna.app.Siigna

/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

/*
paper properties changeable by the user
 */

//change paper size
object setPaperProperties {
  def changeSize(increase : Boolean) = {
    val step = math.sqrt(2.0)
    val min = Siigna.double("printFormatMin").get
    val max = Siigna.double("printFormatMax").get
    if(increase) {
      Siigna("printFormatMin") = min * step
      Siigna("printFormatMax") = max * step
      println(Siigna.double("printFormatMin").get)
      println(Siigna.double("printFormatMax").get)
    }
    else {
      Siigna("printFormatMin") = min
      Siigna("printFormatMax") = max
    }
  }
  //change paper scale
  def changeScale(increase : Boolean) = {
    if(increase) println("increase")
    else println("decrease")
  }
}
