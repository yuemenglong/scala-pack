package io.github.yuemenglong.pack.jvm.op

import io.github.yuemenglong.pack.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpExt {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case 0xC4 => new OpWide(reader, cf, method, lineNo, code)
      case 0xC5 => new OpMultiANewArray(reader, cf, method, lineNo, code)
      case 0xC6 => new OpIfNull(reader, cf, method, lineNo, code)
      case 0xC7 => new OpIfNull(reader, cf, method, lineNo, code)
      case 0xC8 => new OpGotoW(reader, cf, method, lineNo, code)
      case 0xC9 => new OpJsrW(reader, cf, method, lineNo, code)
    }
  }
}

class OpWide(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val realCode: Int = (reader.readByte() + 256) % 256
  val varnum: Short = reader.readShort()
  var n: Short = _

  val realName: String = realCode match {
    case c if 0x15 <= c && c <= 0x19 => "ilfda".charAt(realCode - 0x15) + "load"
    case c if 0x36 <= c && c <= 0x3A => "ilfda".charAt(realCode - 0x36) + "store"
    case 0x84 =>
      n = reader.readShort()
      "iinc"
    case 0xA9 => "ret"
  }

  override val opName = s"${realName}_w ${varnum} ${
    realName match {
      case "iinc" => n.toString
      case _ => ""
    }
  }"

  override def proc(ctx: ThreadCtx): Unit = ???

}

class OpMultiANewArray(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val index: Short = reader.readShort()
  val n: Byte = reader.readByte()

  override val opName = s"multianewarray ${cpc(index).name}"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpIfNull(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val offset: Short = reader.readShort()
  val pos: Int = offset + lineNo

  override val opName = opCode match {
    case 0xC6 => s"ifnull ${pos}"
    case 0xC7 => s"ifnonnull ${pos}"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val v = ctx.pop()
    val jmp = (opCode == 0xC6 && v == null) || (opCode == 0xC7 && v != null)
    if (jmp) {
      ctx.goto(pos)
    }
  }
}

class OpGotoW(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val offset: Int = reader.readInt()

  override val opName = s"gotow ${offset}"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpJsrW(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val offset: Int = reader.readInt()

  override val opName = s"jsrw ${offset}"

  override def proc(ctx: ThreadCtx): Unit = ???
}
