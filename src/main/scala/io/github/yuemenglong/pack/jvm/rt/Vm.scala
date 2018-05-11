package io.github.yuemenglong.pack.jvm.rt

import io.github.yuemenglong.pack.jvm.common.{Kit, UnreachableException}
import io.github.yuemenglong.pack.jvm.struct.MethodInfo
import io.github.yuemenglong.pack.jvm.common.Kit
import io.github.yuemenglong.pack.jvm.struct.MethodInfo

/**
  * Created by <yuemenglong@126.com> on 2018/2/13.
  */
object Vm {

  val rt: RuntimeCtx = new RuntimeCtx

  def init(): Unit = {
    // 调用initializeSystemClass
    rt.load("java/lang/Class")
    val m = rt.load("java/lang/System").method("initializeSystemClass")
    run(m)
    Kit.debug("Finish Init")
  }

  def run(ctx: ThreadCtx): Any = {
    def isFinish: Boolean = ctx.frames.isEmpty

    while (!isFinish) {
      Kit.debug(ctx)
      val code = ctx.code()
      Kit.debug(f"[${code.cf.simpleName}:${code.method.name}] [${code.lineNo}] ${code}")
      ctx.inc()
      code.proc(ctx)
    }
    rt.finishThread(ctx)
    ctx.stack.nonEmpty match {
      case true => ctx.stack.last
      case false => Unit
    }
  }

  def run(method: MethodInfo, vt: Map[Int, Any] = Map()): Any = {
    run(rt.createThread(method, vt))
  }
}
