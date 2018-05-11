package io.github.yuemenglong.pack.jvm.nativ

import io.github.yuemenglong.pack.jvm.common.Kit
import io.github.yuemenglong.pack.jvm.common.Kit
import io.github.yuemenglong.pack.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/19.
  */

class Obj(val cf: ClassFile, obj: Object = null) extends Ref {
  private[nativ] var fields: Map[String, Any] = Kit.getCfFields(cf).map(f => (f.name, Kit.defaultFieldValue(f))).toMap

  def get(key: String): Any = fields(key)

  def set(key: String, value: Any): Unit = fields += (key -> value)

  def call(method: String, params: Any*): Unit = {}

  override def toString: String = {
    s"[${cf.simpleName}]@[${id}]"
  }
}
