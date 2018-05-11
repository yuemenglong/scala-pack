package io.github.yuemenglong.pack.jvm.rt

import io.github.yuemenglong.pack.jvm.nativ._
import io.github.yuemenglong.pack.jvm.nativ.{ClsA, ClsAT, ClsT, Obj}
import io.github.yuemenglong.pack.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/20.
  */
trait RuntimeCls {

  val clazz: Obj

  def putStatic(key: String, value: Any): Unit

  def getStatic(key: String): Any
}

class RuntimeClsT(val cf: ClassFile) extends RuntimeCls {
  private var statics: Map[String, Any] = cf.fields.filter(f => {
    f.accessFlags.contains("ACC_STATIC")
  }).map(f => (f.name, null)).toMap

  val clazz: Obj = new ClsT(cf)

  def putStatic(key: String, value: Any): Unit = statics += (key -> value)

  def getStatic(key: String): Any = statics(key)
}

class RuntimeClsA(t: String, dim: Int) extends RuntimeCls {
  override val clazz = new ClsA(t, dim)

  override def putStatic(key: String, value: Any): Unit = ???

  override def getStatic(key: String) = ???
}

class RuntimeClsAT(cf: ClassFile, dim: Int) extends RuntimeCls {
  override val clazz = new ClsAT(cf, dim)

  override def putStatic(key: String, value: Any): Unit = ???

  override def getStatic(key: String) = ???
}
