package com.siigna.module.cad.helpers

import com.siigna._

class ZoomExtends extends Module {

  val stateMap : StateMap = Map(

    'Start-> {
      case _ => {
        View.zoomExtends
        Siigna display "Zooming to extends"
        End
      }
    }
  )
}