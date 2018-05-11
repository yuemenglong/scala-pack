package io.github.yuemenglong.pack.jvm.nativ

import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.ClassFile
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */
object Ref {
  private var counter: Int = 0

  def inc(): Int = {
    counter += 1
    counter
  }
}

trait Ref {
  val id: Int = Ref.inc()
  var monitor: Int = 0
  var tid: Long = 0

  def monitorEnter(ctx: ThreadCtx): Boolean = {
    monitor match {
      case _ if monitor > 0 => tid match {
        case ctx.id =>
          monitor += 1
          true
        case _ => false
      }
      case 0 =>
        tid = ctx.id
        monitor += 1
        true
    }
  }

  def monitorExit(ctx: ThreadCtx): Unit = {
    monitor -= 1
    if (monitor == 0) {
      tid = 0
    }
  }

  def debug: String = this match {
    case obj: Obj =>
      val content = obj.fields.map { case (key, value) =>
        val vs = value match {
          case ref: Ref => ref.debug
          case _ => value.toString
        }
        s"${key}:${vs}"
      }.mkString(",")
      s"{${content}"
  }
}
