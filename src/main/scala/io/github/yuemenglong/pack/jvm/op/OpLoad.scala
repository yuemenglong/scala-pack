package io.github.yuemenglong.pack.jvm.op

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.nativ.Arr
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.nativ.Arr
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpLoad {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x15 <= c && c <= 0x2D => new OpLoad(reader, cf, method, lineNo, code)
      case c if 0x2E <= c && c <= 0x35 => new OpALoad(reader, cf, method, lineNo, code)
    }
  }
}

class OpLoad(reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int
            ) extends Op {
  val index = opCode match {
    case c if 0x15 <= c && c <= 0x19 => reader.readByte().toInt
    case c if 0x1A <= c && c <= 0x2D => (opCode - 0x1A) % 4
  }
  val prefix = opCode match {
    case c if 0x15 <= c && c <= 0x19 => "ilfda".charAt(opCode - 0x15)
    case c if 0x1A <= c && c <= 0x2D => "ilfda".charAt((opCode - 0x1A) / 4)
  }
  override val opName = {
    s"${prefix}load_${index}"
  }

  override def proc(ctx: ThreadCtx): Unit = ctx.push(ctx.get(index))
}

class OpALoad(reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              val lineNo: Int,
              val opCode: Int
             ) extends Op {

  val prefix = "ilfdabcs".charAt(opCode - 0x2E)

  override val opName = {
    s"${prefix}aload"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val index = ctx.pop().toString.toInt
    val arr = ctx.pop().asInstanceOf[Arr]
    val value = arr.load(index)
    ctx.push(value)
  }
}
