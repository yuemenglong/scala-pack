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
object OpStack {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x57 <= c && c <= 0x58 => new OpPop(reader, cf, method, lineNo, code)
      case c if 0x59 <= c && c <= 0x5B => new OpDup(reader, cf, method, lineNo, code)
      case c if 0x5C <= c && c <= 0x5E => new OpDup2(reader, cf, method, lineNo, code)
      case c if 0x5F <= c && c <= 0x5F => new OpSwap(reader, cf, method, lineNo, code)
    }
  }
}

class OpPop(reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val lineNo: Int,
            val opCode: Int
           ) extends Op {
  override val opName = {
    opCode match {
      case 0x57 => "pop"
      case 0x58 => "pop2"
    }
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val top = ctx.pop()
    val is8Byte = top.isInstanceOf[Long] || top.isInstanceOf[Double]
    opCode match {
      case 0x57 => require(!is8Byte)
      case 0x58 => require(is8Byte)
    }
  }
}


class OpDup(val reader: StreamReader,
            override val cf: ClassFile,
            override val method: MethodInfo,
            val lineNo: Int,
            val opCode: Int
           ) extends Op {
  val offset: Int = opCode - 0x59

  override val opName = {
    opCode match {
      case 0x59 => "dup"
      case 0x5A => "dup_x1"
      case 0x5B => "dup_x2"
    }
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val idx = opCode - 0x59
    ctx.insert(idx, ctx.peek(0))
  }
}

class OpDup2(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int
            ) extends Op {
  val offset: Int = opCode - 0x5C

  override val opName = {
    opCode match {
      case 0x5C => "dup2"
      case 0x5D => "dup2_x1"
      case 0x5E => "dup2_x2"
    }
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}


class OpSwap(val reader: StreamReader,
             override val cf: ClassFile,
             override val method: MethodInfo,
             val lineNo: Int,
             val opCode: Int
            ) extends Op {
  val offset: Int = opCode - 0x5C

  override val opName = {
    "swap"
  }

  override def proc(ctx: ThreadCtx): Unit = ???
}
