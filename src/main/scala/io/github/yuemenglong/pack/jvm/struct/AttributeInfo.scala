package io.github.yuemenglong.pack.jvm.struct

import io.github.yuemenglong.pack.jvm.attribute.method.{CodeAttribute, LineNumberTableAttribute, LocalVariableTableAttribute, SignatureAttribute}
import io.github.yuemenglong.pack.jvm.common.{JvmItem, StreamReader}
import io.github.yuemenglong.pack.jvm.attribute.method.{CodeAttribute, LineNumberTableAttribute, LocalVariableTableAttribute, SignatureAttribute}
import io.github.yuemenglong.pack.jvm.common.{JvmItem, StreamReader}

object AttributeInfo {
  def load(reader: StreamReader, cf: ClassFile, method: MethodInfo = null): AttributeInfo = {
    val attribute_name_index = reader.readShort()
    val attribute_length = reader.readInt()
    val name = s"${cf.constant_pool(attribute_name_index).asInstanceOf[ValuedCpInfo].value}"
    name match {
      case "Code" => new CodeAttribute(reader, cf, method, attribute_name_index, attribute_length)
      case "LocalVariableTable" => new LocalVariableTableAttribute(reader, cf, method, attribute_name_index, attribute_length)
      case "LineNumberTable" => new LineNumberTableAttribute(reader, cf, method, attribute_name_index, attribute_length)
      case "Signature" => new SignatureAttribute(reader, cf, method, attribute_name_index, attribute_length)
      case _ => new OtherAttribute(reader, cf, attribute_name_index, attribute_length)
    }
  }
}

trait AttributeInfo extends JvmItem {
  val attribute_name_index: Short
  val attribute_length: Int

  def name: String = s"${cpv(attribute_name_index).value}"

  override def toString = name + "???"
}

trait MethodAttributeChild extends AttributeInfo {
  val method: MethodInfo
}

class OtherAttribute(reader: StreamReader,
                     override val cf: ClassFile,
                     override val attribute_name_index: Short,
                     override val attribute_length: Int
                    ) extends AttributeInfo {
  val bytes: Array[Byte] = reader.readBytes(attribute_length)
}






