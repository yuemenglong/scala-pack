package io.github.yuemenglong.pack.jvm.rt

import io.github.yuemenglong.pack.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/14.
  */

class Frame(val method: MethodInfo, val bp: Int, map: Map[Int, Any] = Map()) {
  var codePos: Int = 0
  var localVariable: Map[Int, Any] = map
}