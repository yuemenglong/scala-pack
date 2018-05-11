package io.github.yuemenglong.pack.jvm.rt

import io.github.yuemenglong.pack.jvm.struct.MethodInfo

import scala.collection.mutable.ArrayBuffer

object ThreadCtx {
  private var counter: Int = 0

  def inc(): Int = {
    counter += 1
    counter
  }
}

/**
  * Created by <yuemenglong@126.com> on 2018/2/14.
  */
class ThreadCtx(m: MethodInfo, val rt: RuntimeCtx, vt: Map[Int, Any] = Map()) {
  val id: Int = ThreadCtx.inc()
  var frames: ArrayBuffer[Frame] = new ArrayBuffer[Frame]()
  var stack: ArrayBuffer[Any] = new ArrayBuffer[Any]()
  frames += new Frame(m, stack.size, vt)

  def pc: Int = method.codes(frame.codePos).lineNo

  def method: MethodInfo = frame.method

  def frame: Frame = frames.last

  def push(value: Any): Unit = stack += value

  def pop(): Any = {
    require(ss > 0)
    val ret = stack.last
    stack = stack.slice(0, stack.length - 1)
    ret
  }

  def ss: Int = stack.length - frame.bp

  def peek(idx: Int): Any = {
    require(ss > idx)
    stack(stack.length - 1 - idx)
  }

  def insert(idx: Int, value: Any): Any = {
    require(ss > idx)
    val (a, b) = stack.splitAt(stack.length - idx - 1)
    stack = a ++ Array(value) ++ b
  }

  def get(idx: Int): Any = frame.localVariable(idx)

  def set(idx: Int, value: Any): Unit = frame.localVariable += (idx -> value)

  def code() = method.codes(frame.codePos)

  def call(method: MethodInfo, params: Map[Int, Any]): Unit = {
    val frame = new Frame(method, stack.size, params)
    frames += frame
  }

  def ret() = {
    frames -= frame
  }

  def inc(): Unit = {
    frame.codePos += 1
  }

  def goto(lineNo: Int): Unit = {
    frame.codePos = method.codes.zipWithIndex.find(_._1.lineNo == lineNo).get._2
  }

  override def toString = {
    val p = s"\t[${id}|Pc:${method.name}] ${pc}"
    val l = frame.localVariable match {
      case v if v.nonEmpty => v.toArray.sortBy(_._1)
        .map { case (idx, value) =>
          s"\t[Local] [${idx}] ${value}"
        }.mkString("\n")
      case _ => "\t[Local-None]"
    }
    val m = s"\t[Stack-Size] ${ss}"
    val s = stack.nonEmpty match {
      case true => stack.map(v => s"\t[Stack] ${v}").mkString("\n")
      case false => "\t[Stack-None]"
    }
    s"${p}\n${l}\n${m}\n${s}"
  }
}


