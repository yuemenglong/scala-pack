package io.github.yuemenglong.pack.jvm.common

import java.io.{BufferedInputStream, InputStream}
import java.nio.ByteBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/8.
  */
class StreamReader(is: InputStream) {
  val buffer = new BufferedInputStream(is)
  private var pos0 = 0
  //  var s = seq

  private def readT[T](len: Int, fn: ByteBuffer => T): T = {
    val wrap = ByteBuffer.wrap(readBytes(len))
    fn(wrap)
  }

  def pos: Int = pos0

  def readBytes(len: Int): Array[Byte] = {
    val ret = new Array[Byte](len)
    val l = buffer.read(ret)
    require(l == len)
    pos0 += l
    ret
    //    val arr = s.slice(0, len).toArray
    //    s = s.drop(len)
    //    arr
  }

  def readByte(): Byte = readT(1, _.get())

  def readShort(): Short = readT(2, _.getShort())

  def readInt(): Int = readT(4, _.getInt())

  def readLong(): Long = readT(8, _.getLong())

  def readFloat(): Float = readT(4, _.getFloat())

  def readDouble(): Double = readT(8, _.getDouble())

  def readString(len: Int): String = new String(readBytes(len))

}
