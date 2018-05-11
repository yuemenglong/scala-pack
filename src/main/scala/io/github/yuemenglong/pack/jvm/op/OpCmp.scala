package io.github.yuemenglong.pack.jvm.op

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.nativ.{Num, Obj}
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.nativ.{Num, Obj}
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpCmp {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x94 <= c && c <= 0x98 => new OpCmp(reader, cf, method, lineNo, code)
      case c if 0x99 <= c && c <= 0x9E => new OpCmp0(reader, cf, method, lineNo, code)
      case c if 0x9F <= c && c <= 0xA4 => new OpCmpI(reader, cf, method, lineNo, code)
      case c if 0xA5 <= c && c <= 0xA6 => new OpCmpA(reader, cf, method, lineNo, code)
    }
  }
}

class OpCmp(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  override val opName = opCode match {
    case 0x94 => "lcmp"
    case 0x95 => "fcmpl"
    case 0x96 => "fcmpg"
    case 0x97 => "dcmpl"
    case 0x98 => "dcmpg"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val b = ctx.pop()
    val a = ctx.pop()
    val res = opCode match {
      case 0x94 => Num.cmp(a, b)
      case 0x95 => Num.cmp(a, b, -1)
      case 0x96 => Num.cmp(a, b, 1)
      case 0x97 => Num.cmp(a, b, -1)
      case 0x98 => Num.cmp(a, b, 1)
    }
    ctx.push(res)
  }
}

class OpCmp0(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val offset: Short = reader.readShort()
  val pos = offset + lineNo

  val fn: Int => Boolean = opCode match {
    case 0x99 => _ == 0
    case 0x9A => _ != 0
    case 0x9B => _ < 0
    case 0x9C => _ >= 0
    case 0x9D => _ > 0
    case 0x9E => _ <= 0
  }
  val postfix = opCode match {
    case 0x99 => "eq"
    case 0x9A => "ne"
    case 0x9B => "lt"
    case 0x9C => "ge"
    case 0x9D => "gt"
    case 0x9E => "le"
  }
  override val opName = {
    s"if${postfix} ${pos}"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val a = ctx.pop().toString.toInt
    if (fn(a)) {
      ctx.goto(pos)
    }
  }
}

class OpCmpI(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val offset: Short = reader.readShort()

  val pos = offset + lineNo

  val fn: (Int, Int) => Boolean = opCode match {
    case 0x9F => (a: Int, b: Int) => a == b
    case 0xA0 => (a: Int, b: Int) => a != b
    case 0xA1 => (a: Int, b: Int) => a < b
    case 0xA2 => (a: Int, b: Int) => a >= b
    case 0xA3 => (a: Int, b: Int) => a > b
    case 0xA4 => (a: Int, b: Int) => a <= b
  }
  val postfix = opCode match {
    case 0x9F => "eq"
    case 0xA0 => "ne"
    case 0xA1 => "lt"
    case 0xA2 => "ge"
    case 0xA3 => "gt"
    case 0xA4 => "le"
  }
  override val opName = {
    s"if_icmp${postfix} ${pos}"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val b = Num.toInt(ctx.pop())
    val a = Num.toInt(ctx.pop())
    if (fn(a, b)) {
      ctx.goto(pos)
    }
  }
}


class OpCmpA(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val offset: Short = reader.readShort()
  val pos: Int = offset + lineNo
  val fn: (AnyRef, AnyRef) => Boolean = opCode match {
    case 0xA5 => (a: AnyRef, b: AnyRef) => a == b
    case 0xA6 => (a: AnyRef, b: AnyRef) => a != b
  }
  val postfix = opCode match {
    case 0xA5 => "eq"
    case 0xA6 => "ne"
  }

  override val opName = {
    s"if_acmp${postfix} ${pos}"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val a = ctx.pop().asInstanceOf[Obj]
    val b = ctx.pop().asInstanceOf[Obj]
    val jmp = opCode match {
      case 0xA5 => a == b
      case 0xA6 => a != b
    }
    if (jmp) {
      ctx.goto(pos)
    }
  }
}
