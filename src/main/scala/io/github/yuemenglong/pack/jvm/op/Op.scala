package io.github.yuemenglong.pack.jvm.op

import io.github.yuemenglong.pack.jvm.common.{JvmItem, StreamReader}
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.{JvmItem, StreamReader}
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer


trait Op extends JvmItem {
  val method: MethodInfo
  val opCode: Int
  val lineNo: Int

  val opName: String

  def proc(ctx: ThreadCtx)

  override def toString: String = opName
}

object Op {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, length: Int): Array[Op] = {
    val startPos = reader.pos
    val endPos = reader.pos + length
    val ret = new ArrayBuffer[Op]()
    while (reader.pos < endPos) {
      val lineNo = reader.pos - startPos
      val code = (reader.readByte() + 256) % 256
      val op = code match {
        case c if 0x00 <= c && c <= 0x14 => OpConst.load(reader, cf, method, lineNo, code)
        case c if 0x15 <= c && c <= 0x35 => OpLoad.load(reader, cf, method, lineNo, code)
        case c if 0x36 <= c && c <= 0x56 => OpStore.load(reader, cf, method, lineNo, code)
        case c if 0x57 <= c && c <= 0x5F => OpStack.load(reader, cf, method, lineNo, code)
        case c if 0x60 <= c && c <= 0x84 => OpMath.load(reader, cf, method, lineNo, code)
        case c if 0x85 <= c && c <= 0x93 => OpConvert.load(reader, cf, method, lineNo, code)
        case c if 0x94 <= c && c <= 0xA6 => OpCmp.load(reader, cf, method, lineNo, code)
        case c if 0xA7 <= c && c <= 0xB1 => OpCtrl.load(reader, cf, method, lineNo, code)
        case c if 0xB2 <= c && c <= 0xC3 => OpRef.load(reader, cf, method, lineNo, code)
        case c if 0xC4 <= c && c <= 0xC9 => OpExt.load(reader, cf, method, lineNo, code)
        case _ => new OpOther(reader, cf, method, lineNo, code, reader.readBytes(endPos - reader.pos))
      }
      ret += op
    }
    ret.toArray
  }
}

class OpOther(reader: StreamReader,
              override val cf: ClassFile,
              override val method: MethodInfo,
              override val lineNo: Int,
              val opCode: Int,
              val bytes: Array[Byte]
             ) extends Op {
  override val opName: String = (Array(opCode.toByte) ++ bytes).map(b => f"${b}%02X").mkString("-")

  override def proc(ctx: ThreadCtx): Unit = {
    throw new RuntimeException(f"Op ${bytes(0)}%02X Not Implement")
  }
}
