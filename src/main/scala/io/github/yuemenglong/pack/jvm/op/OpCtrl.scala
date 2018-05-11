package io.github.yuemenglong.pack.jvm.op

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}


/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpCtrl {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case 0xA7 => new OpGoto(reader, cf, method, lineNo, code)
      case 0xA8 => new OpJsr(reader, cf, method, lineNo, code)
      case 0xA9 => new OpRet(reader, cf, method, lineNo, code)
      case 0xAA => new OpTableSwitch(reader, cf, method, lineNo, code)
      case 0xAB => new OpLookupSwitch(reader, cf, method, lineNo, code)
      case c if 0xAC <= c && c <= 0xB1 => new OpReturn(reader, cf, method, lineNo, code)
    }
  }
}

class OpGoto(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val offset: Short = reader.readShort()
  val pos = offset + lineNo
  override val opName = s"goto ${lineNo}"

  override def proc(ctx: ThreadCtx): Unit = {
    ctx.goto(pos)
  }
}

class OpJsr(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {

  val offset: Short = reader.readShort()
  override val opName = s"jsr ${offset}"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpRet(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val varnum: Byte = reader.readByte()
  override val opName = s"ret ${varnum}"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpTableSwitch(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val pad: Array[Byte] = reader.readBytes(3 - lineNo % 4)
  val dft: Int = reader.readInt() + lineNo
  val low: Int = reader.readInt()
  val high: Int = reader.readInt()
  val offsets: Array[(Int, Int)] = (low to high).map(idx => (idx, reader.readInt() + lineNo)).toArray
  override val opName = s"tableswitch [${dft}|${low}|${high}] [${offsets.map(p => s"${p._1}->${p._2}").mkString(",")}]"

  override def proc(ctx: ThreadCtx): Unit = ???
}

class OpLookupSwitch(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val pad: Array[Byte] = reader.readBytes(3 - lineNo % 4)
  val dft: Int = reader.readInt() + lineNo
  val npairs: Int = reader.readInt()
  val offset_pairs: Array[(Int, Int)] = (1 to npairs).map(_ => (reader.readInt(), reader.readInt() + lineNo)).toArray
  override val opName = s"lookupswitch [${dft}|${npairs}] [${offset_pairs.map(p => s"${p._1}->${p._2}").mkString(",")}]"

  override def proc(ctx: ThreadCtx): Unit = ???
}


class OpReturn(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val prefix: String = opCode match {
    case 0xAC => "i"
    case 0xAD => "l"
    case 0xAE => "f"
    case 0xAF => "d"
    case 0xB0 => "a"
    case 0xB1 => ""
  }
  override val opName = s"${prefix}return"

  override def proc(ctx: ThreadCtx): Unit = {
    ctx.ret()
  }
}