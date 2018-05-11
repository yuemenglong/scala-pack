package io.github.yuemenglong.pack.jvm.struct

import io.github.yuemenglong.pack.jvm.common.{AccessFlagName, JvmItem, StreamReader}
import io.github.yuemenglong.pack.jvm.common.{AccessFlagName, JvmItem, StreamReader}

class ClassFile(reader: StreamReader) extends JvmItem with AccessFlagName {
  val cf: ClassFile = this

  val magic: Int = reader.readInt()
  require(magic == 0xCAFEBABE)
  val minor_version: Short = reader.readShort()
  val major_version: Short = reader.readShort()
  val constant_pool_count: Short = reader.readShort()
  val constant_pool: Array[CpInfo] = CpInfo.load(reader, this, constant_pool_count)
  val access_flags: Short = reader.readShort()
  val this_class: Short = reader.readShort()
  val super_class: Short = reader.readShort()
  val interfaces_count: Short = reader.readShort()
  val interfaces: Array[Short] = (1 to interfaces_count).map(_ => {
    reader.readShort()
  }).toArray
  val fields_count: Short = reader.readShort()
  val fields: Array[FieldInfo] = (1 to fields_count).map(_ => {
    new FieldInfo(reader, this)
  }).toArray
  val methods_count: Short = reader.readShort()
  val methods: Array[MethodInfo] = (1 to methods_count).map(_ => {
    new MethodInfo(reader, this)
  }).toArray
  val attribute_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attribute_count).map(_ => {
    AttributeInfo.load(reader, this)
  }).toArray

  override def toString: String = {
    Array(
      s"${accessFlags.mkString(",")} [${name}] Extends [${sup}] Implements [${impls.mkString(",")}]",
      s"[Interface] ${impls.mkString(",")}",
      methods.mkString("\n"),
      fields.mkString("\n"),
      "[ATTRIBUTE]",
      attributes.mkString("\n")
    ).mkString("\n")
  }

  def todo(): String = {
    attributes.mkString("\n") + "\n" +
      methods.map(_.todo).mkString("\n") + "\n" +
      fields.map(_.todo).mkString("\n")
  }

  def name: String = cp(this_class).asInstanceOf[ConstantClassInfo].name

  def simpleName: String = name.split("/").last

  def sup: String = {
    super_class match {
      case 0 => null
      case _ => cp(super_class).asInstanceOf[ConstantClassInfo].name
    }
  }

  def main(): MethodInfo = {
    method("main")
  }

  def method(name: String): MethodInfo = {
    methods(name) match {
      case arr if arr.length > 0 => arr(0)
      case _ => null
    }
  }

  def methods(name: String): Array[MethodInfo] = {
    methods.filter(m => m.name == name)
  }

  def method(name: String, descriptor: String): MethodInfo = {
    methods.find(m => m.name == name && m.descriptor == descriptor) match {
      case Some(m) => m
      case None => null
    }
  }

  def field(name: String): Array[FieldInfo] = {
    fields.filter(m => m.name == name)
  }

  def field(name: String, descriptor: String): FieldInfo = {
    fields.find(m => m.name == name && m.descriptor == descriptor) match {
      case Some(m) => m
      case None => null
    }
  }

  def impls: Array[String] = interfaces.map(i => {
    cpc(i).name
  })

  override def accessMaskMap = Map(
    0x0001 -> "ACC_PUBLIC",
    0x0010 -> "ACC_FINAL",
    0x0020 -> "ACC_SUPER",
    0x0200 -> "ACC_INTERFACE",
    0x0400 -> "ACC_ABSTRACT",
    0x1000 -> "ACC_SYNTHETIC",
    0x2000 -> "ACC_ANNOTATION",
    0x4000 -> "ACC_ENUM"
  )
}
