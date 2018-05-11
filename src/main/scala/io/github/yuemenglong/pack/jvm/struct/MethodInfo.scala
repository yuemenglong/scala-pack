package io.github.yuemenglong.pack.jvm.struct

import io.github.yuemenglong.pack.jvm.attribute.method.{CodeAttribute, SignatureAttribute}
import io.github.yuemenglong.pack.jvm.common.{AccessFlagName, JvmItem, Kit, StreamReader}
import io.github.yuemenglong.pack.jvm.op.Op
import io.github.yuemenglong.pack.jvm.attribute.method.{CodeAttribute, SignatureAttribute}
import io.github.yuemenglong.pack.jvm.common.{AccessFlagName, JvmItem, Kit, StreamReader}
import io.github.yuemenglong.pack.jvm.op.Op

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */
class MethodInfo(reader: StreamReader, val cf: ClassFile) extends JvmItem with AccessFlagName {
  val access_flags: Short = reader.readShort()
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()
  val attributes_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attributes_count).map(_ => {
    AttributeInfo.load(reader, cf, method = this)
  }).toArray

  def name: String = cpv(name_index).value.toString

  def descriptor: String = cpv(descriptor_index).value.toString

  def codes: Array[Op] = code match {
    case _ if code != null => code.code
    case _ => throw new RuntimeException("Unreachable")
  }

  def params: Array[String] = Kit.params(descriptor)

  def returnType: String = {
    val re = """\(.*\)(.*)""".r
    descriptor match {
      case re(t) => t
    }
  }

  def code: CodeAttribute = attributes.find(_.isInstanceOf[CodeAttribute]).orNull.asInstanceOf[CodeAttribute]

  def signatures: Array[SignatureAttribute] = attributes.filter(_.isInstanceOf[SignatureAttribute]).map(_.asInstanceOf[SignatureAttribute])

  override def toString: String = {
    val fn = s"${signatures.map(_.toString).mkString(",")} ${accessFlags.mkString(",")} ${descriptor} ${name}"
    val c = code match {
      case null => ""
      case _ => s"\n${code}"
    }
    s"<Method> ${fn}${c}"
  }

  def todo: String = {
    attributes.filter(a => !(a.isInstanceOf[CodeAttribute] || a.isInstanceOf[SignatureAttribute]))
      .mkString("\n") + "\n" +
      code.todo
  }

  override def accessMaskMap = Map(
    0x0001 -> "ACC_PUBLIC",
    0x0002 -> "ACC_PRIVATE",
    0x0004 -> "ACC_PROTECTED",
    0x0008 -> "ACC_STATIC",
    0x0010 -> "ACC_FINAL",
    0x0020 -> "ACC_SYNCHRONIZED",
    0x0040 -> "ACC_BRIDGE",
    0x0080 -> "ACC_VARARGS",
    0x0100 -> "ACC_NATIVE",
    0x0400 -> "ACC_ABSTRACT",
    0x0800 -> "ACC_STRICT",
    0x1000 -> "ACC_SYNTHETIC"
  )
}
