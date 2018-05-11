package io.github.yuemenglong.pack.jvm.struct

import io.github.yuemenglong.pack.jvm.common.{JvmItem, Kit, StreamReader}
import io.github.yuemenglong.pack.jvm.common.{JvmItem, Kit, StreamReader}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
object CpInfo {
  def load(reader: StreamReader, cf: ClassFile, count: Int): Array[CpInfo] = {
    val ret = new ArrayBuffer[CpInfo]()
    ret += null
    var pos = 1
    while (pos < count) {
      val tag = reader.readByte()
      val info = tag match {
        case 1 => new ConstantUtf8Info(reader, cf)
        case 3 => new ConstantIntegerInfo(reader, cf)
        case 4 => new ConstantFloatInfo(reader, cf)
        case 5 => new ConstantLongInfo(reader, cf)
        case 6 => new ConstantDoubleInfo(reader, cf)
        case 7 => new ConstantClassInfo(reader, cf)
        case 8 => new ConstantStringInfo(reader, cf)
        case 9 => new ConstantFieldrefInfo(reader, cf)
        case 10 => new ConstantMethodrefInfo(reader, cf)
        case 11 => new ConstantInterfaceMethodrefInfo(reader, cf)
        case 12 => new ConstantNameAndTypeInfo(reader, cf)
        case 15 => new ConstantMethodHandleInfo(reader, cf)
        case 16 => new ConstantMethodTypeInfo(reader, cf)
        case 18 => new ConstantInvokeDynamicInfo(reader, cf)
      }
      ret += info
      pos += 1
      if (Array(5, 6).indexOf(tag) >= 0) {
        ret += null
        pos += 1
      }
    }
    ret.toArray
  }

  def debug(arr: Array[CpInfo]): Unit = {
    val str = arr.zipWithIndex.map { case (info, i) =>
      if (info == null) {
        f"[${i}%02d] Null"
      } else {
        f"[${i}%02d] [${info.cpType}] [${info.debug}]"
      }
    }.mkString("\n")
    Kit.debug(str)
  }
}

trait CpInfo extends JvmItem {
  val tag: Byte

  def debug: String

  override def toString: String = s"[${cpType}] ${debug}"

  def cpType: String = {
    this match {
      case _: ConstantUtf8Info => "Utf8"
      case _: ConstantIntegerInfo => "Integer"
      case _: ConstantFloatInfo => "Float"
      case _: ConstantLongInfo => "Long"
      case _: ConstantDoubleInfo => "Double"
      case _: ConstantClassInfo => "Class"
      case _: ConstantStringInfo => "String"
      case _: ConstantFieldrefInfo => "Fieldref"
      case _: ConstantMethodrefInfo => "Methodref"
      case _: ConstantInterfaceMethodrefInfo => "InterfaceMethodref"
      case _: ConstantNameAndTypeInfo => "NameAndType"
      case _: ConstantMethodHandleInfo => "MethodHandle"
      case _: ConstantMethodTypeInfo => "MethodType"
      case _: ConstantInvokeDynamicInfo => "InvokeDynamic"
    }
  }
}

trait ValuedCpInfo extends CpInfo {
  def value: Any

  override def debug: String = value.toString
}

class ConstantUtf8Info(reader: StreamReader, override val cf: ClassFile) extends ValuedCpInfo {
  override val tag: Byte = 1
  val length: Short = reader.readShort()
  val bytes: String = reader.readString(length)

  def value: Any = bytes
}

class ConstantIntegerInfo(reader: StreamReader, override val cf: ClassFile) extends ValuedCpInfo {
  override val tag: Byte = 3
  val bytes: Int = reader.readInt()

  def value: Any = bytes
}

class ConstantFloatInfo(reader: StreamReader, override val cf: ClassFile) extends ValuedCpInfo {
  override val tag: Byte = 4

  val bytes: Float = reader.readFloat()

  def value: Any = bytes
}

class ConstantLongInfo(reader: StreamReader, override val cf: ClassFile) extends ValuedCpInfo {
  override val tag: Byte = 5

  val bytes: Long = reader.readLong()

  def value: Any = bytes
}

class ConstantDoubleInfo(reader: StreamReader, override val cf: ClassFile) extends ValuedCpInfo {
  override val tag: Byte = 6

  val bytes: Double = reader.readDouble()

  def value: Any = bytes
}

class ConstantClassInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 7
  val name_index: Short = reader.readShort()

  def name: String = cpv(name_index).value.toString

  override def debug: String = name
}

class ConstantStringInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 8
  val string_index: Short = reader.readShort()

  def value: String = cpv(string_index).value.toString

  override def debug: String = value.toString
}

class ConstantFieldrefInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 9
  val class_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  def clazz: String = cpc(class_index).name

  def name: String = cp(name_and_type_index).asInstanceOf[ConstantNameAndTypeInfo].name

  def descriptor: String = cp(name_and_type_index).asInstanceOf[ConstantNameAndTypeInfo].descriptor

  override def debug = s"${clazz} ${descriptor} ${name}"
}

class ConstantMethodrefInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 10
  val class_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  def clazz: String = cp(class_index).asInstanceOf[ConstantClassInfo].name

  def name: String = cp(name_and_type_index).asInstanceOf[ConstantNameAndTypeInfo].name

  def descriptor: String = cp(name_and_type_index).asInstanceOf[ConstantNameAndTypeInfo].descriptor

  override def debug = s"${clazz} ${descriptor} ${name}"

}

class ConstantInterfaceMethodrefInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 11
  val class_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  def clazz: String = cpc(class_index).name.toString

  def name: String = cp(name_and_type_index).asInstanceOf[ConstantNameAndTypeInfo].name

  def descriptor: String = cp(name_and_type_index).asInstanceOf[ConstantNameAndTypeInfo].descriptor

  override def debug = s"${clazz} ${descriptor} ${name}"
}

class ConstantNameAndTypeInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 12
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()

  def name: String = cpv(name_index).value.toString

  def descriptor: String = cpv(descriptor_index).value.toString

  override def debug = s"${descriptor} ${name}"
}

class ConstantMethodHandleInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 15
  val reference_kind: Byte = reader.readByte()
  val reference_index: Short = reader.readShort()

  override def debug: String = reference_kind + " | " + cp(reference_index)
}

class ConstantMethodTypeInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 16
  val descriptor_index: Short = reader.readShort()

  override def debug: String = cp(descriptor_index).toString
}

class ConstantInvokeDynamicInfo(reader: StreamReader, override val cf: ClassFile) extends CpInfo {
  override val tag: Byte = 18
  val bootstrap_method_attr_index: Short = reader.readShort()
  val name_and_type_index: Short = reader.readShort()

  override def debug: String = cp(bootstrap_method_attr_index) + " | " + cp(name_and_type_index)
}
