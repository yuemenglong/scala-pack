package io.github.yuemenglong.pack.jvm.struct

import io.github.yuemenglong.pack.jvm.common.{AccessFlagName, JvmItem, StreamReader}
import io.github.yuemenglong.pack.jvm.common.{AccessFlagName, JvmItem, StreamReader}

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
class FieldInfo(reader: StreamReader, val cf: ClassFile) extends JvmItem with AccessFlagName {
  val access_flags: Short = reader.readShort()
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()
  val attributes_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attributes_count).map(_ => {
    AttributeInfo.load(reader, cf)
  }).toArray

  def name: String = cpv(name_index).value.toString

  def descriptor: String = cpv(descriptor_index).value.toString

  override def toString = {
    s"<Field> ${accessFlags.mkString(",")} ${descriptor} ${name}"
  }

  def todo = {
    attributes.mkString("\n")
  }

  override def accessMaskMap: Map[Int, String] = Map(
    0x0001 -> "ACC_PUBLIC",
    0x0002 -> "ACC_PRIVATE",
    0x0004 -> "ACC_PROTECTED",
    0x0008 -> "ACC_STATIC",
    0x0010 -> "ACC_FINAL",
    0x0040 -> "ACC_VOLATILE",
    0x0080 -> "ACC_TRANSIENT",
    0x1000 -> "ACC_SYNTHETIC",
    0x4000 -> "ACC_ENUM"
  )
}
