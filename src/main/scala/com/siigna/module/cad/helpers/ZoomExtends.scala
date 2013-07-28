package com.siigna.module.cad.helpers

import com.siigna._
import com.siigna.module.Module

class ZoomExtends extends Module{

  val stateMap : StateMap = Map(

    'Start-> {
      case _ => {
        View.zoomExtends()
        End
      }
    }
  )
}