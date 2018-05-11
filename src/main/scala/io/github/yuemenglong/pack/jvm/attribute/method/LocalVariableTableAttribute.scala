package io.github.yuemenglong.pack.jvm.attribute.method

import io.github.yuemenglong.pack.jvm.common.{JvmItem, StreamReader}
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodAttributeChild, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.{JvmItem, StreamReader}
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodAttributeChild, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
class LocalVariableTableAttribute(reader: StreamReader,
                                  override val cf: ClassFile,
                                  override val method: MethodInfo,
                                  override val attribute_name_index: Short,
                                  override val attribute_length: Int
                                 ) extends MethodAttributeChild {
  val local_variable_table_length: Short = reader.readShort()
  val local_variable_table: Array[LocalVariableTable] = (1 to local_variable_table_length).map(_ => {
    new LocalVariableTable(reader, cf)
  }).toArray

  override def toString = {
    s"${name} ${local_variable_table_length}\n" +
      local_variable_table.map(_.toString).mkString("\n")
  }
}

class LocalVariableTable(reader: StreamReader,
                         override val cf: ClassFile
                        ) extends JvmItem {
  val start_pc: Short = reader.readShort()
  val length: Short = reader.readShort()
  val name_index: Short = reader.readShort()
  val descriptor_index: Short = reader.readShort()
  val index: Short = reader.readShort()

  def name = cf.constant_pool(name_index).toString

  def descriptor = cf.constant_pool(descriptor_index).toString

  override def toString = {
    s"${descriptor} ${name} ${index}"
  }
}
