package io.github.yuemenglong.pack.jvm.op

import io.github.yuemenglong.pack.jvm.common.{StreamReader, Types}
import io.github.yuemenglong.pack.jvm.nativ.Num
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.nativ.Num
import io.github.yuemenglong.pack.jvm.rt.ThreadCtx
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}

import scala.reflect.{ClassTag, classTag}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
object OpMath {
  def load(reader: StreamReader, cf: ClassFile,
           method: MethodInfo, lineNo: Int, code: Int): Op = {
    code match {
      case c if 0x60 <= c && c <= 0x77 => new OpMath(reader, cf, method, lineNo, code)
      case c if 0x78 <= c && c <= 0x83 => new OpMath2(reader, cf, method, lineNo, code)
      case c if 0x84 <= c && c <= 0x84 => new OpInc(reader, cf, method, lineNo, code)
    }
  }
}

class OpMath(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val prefix = "ilfd".charAt(opCode % 4)

  val op = opCode match {
    case c if 0x60 <= c && c <= 0x63 => "add"
    case c if 0x64 <= c && c <= 0x67 => "sub"
    case c if 0x68 <= c && c <= 0x6B => "mul"
    case c if 0x6C <= c && c <= 0x6F => "div"
    case c if 0x70 <= c && c <= 0x73 => "rem"
    case c if 0x74 <= c && c <= 0x77 => "neg"
  }

  override val opName = {
    s"${prefix}${op}"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val top = ctx.pop()
    val res = op match {
      case "add" => Num.add(ctx.pop(), top)
      case "sub" => Num.sub(ctx.pop(), top)
      case "mul" => Num.mul(ctx.pop(), top)
      case "div" => Num.div(ctx.pop(), top)
      case "rem" => Num.rem(ctx.pop(), top)
      case "neg" => Num.neg(top)
    }
    ctx.push(res)
  }
}

class OpMath2(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val prefix = "il".charAt(opCode % 2)

  val fn = opCode match {
    case c if 0x78 <= c && c <= 0x79 => "shl"
    case c if 0x7A <= c && c <= 0x7B => "shr"
    case c if 0x7C <= c && c <= 0x7D => "ushr"
    case c if 0x7E <= c && c <= 0x7F => "and"
    case c if 0x80 <= c && c <= 0x81 => "or"
    case c if 0x82 <= c && c <= 0x83 => "xor"
  }

  override val opName = {
    s"${prefix}${fn}"
  }

  override def proc(ctx: ThreadCtx): Unit = {
    val b = ctx.pop()
    val a = ctx.pop()
    val res = fn match {
      case "shl" => Num.shl(a, b)
      case "shr" => Num.shr(a, b)
      case "ushr" => Num.ushr(a, b)
      case "and" => Num.and(a, b)
      case "or" => Num.or(a, b)
      case "xor" => Num.xor(a, b)
    }
    ctx.push(res)
  }
}

class OpInc(reader: StreamReader, val cf: ClassFile, val method: MethodInfo, val lineNo: Int, val opCode: Int) extends Op {
  val varnum = reader.readByte()
  val n = reader.readByte()
  override val opName = s"iinc [${varnum}] ${n}"

  override def proc(ctx: ThreadCtx): Unit = {
    var res = ctx.get(varnum).asInstanceOf[Int]
    res += n.toInt
    ctx.set(varnum, res)
  }
}
