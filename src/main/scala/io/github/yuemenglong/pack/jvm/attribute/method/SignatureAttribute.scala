package io.github.yuemenglong.pack.jvm.attribute.method

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.struct.{AttributeInfo, ClassFile, MethodAttributeChild, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodAttributeChild, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */
class SignatureAttribute(reader: StreamReader,
                         override val cf: ClassFile,
                         override val method: MethodInfo,
                         override val attribute_name_index: Short,
                         override val attribute_length: Int
                        ) extends MethodAttributeChild {
  val signature_index: Short = reader.readShort()

  override def toString = {
    s"${name} ${cf.constant_pool(signature_index)}"
  }
}
