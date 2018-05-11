package io.github.yuemenglong.pack.jvm.attribute.method

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.op.Op
import io.github.yuemenglong.pack.jvm.struct.{AttributeInfo, ClassFile, MethodAttributeChild, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.op.Op
import io.github.yuemenglong.pack.jvm.struct.{AttributeInfo, ClassFile, MethodAttributeChild, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
class CodeAttribute(reader: StreamReader,
                    override val cf: ClassFile,
                    override val method: MethodInfo,
                    override val attribute_name_index: Short,
                    override val attribute_length: Int
                   ) extends MethodAttributeChild {
  require(method != null)
  val max_stack: Short = reader.readShort()
  val max_locals: Short = reader.readShort()
  val code_length: Int = reader.readInt()
  //  val code: Array[Byte] = reader.readBytes(code_length)
  val code: Array[Op] = Op.load(reader, cf, method, code_length)
  val exception_table_length: Short = reader.readShort()
  val exception_table: Array[Any] = (1 to exception_table_length).map(_ => {
    new ExceptionTable(reader, cf)
  }).toArray
  val attributes_count: Short = reader.readShort()
  val attributes: Array[AttributeInfo] = (1 to attributes_count).map(_ => {
    AttributeInfo.load(reader, cf)
  }).toArray

  override def toString: String = {
    code.zipWithIndex.map(p => s"[${p._2}] ${p._1}").mkString("\n")
  }

  def todo(): String = {
    attributes.mkString("\n")
  }
}

class ExceptionTable(reader: StreamReader, cf: ClassFile) {
  val start_pc: Short = reader.readShort()
  val end_pc: Short = reader.readShort()
  val handler_pc: Short = reader.readShort()
  val catch_type: Short = reader.readShort()
}