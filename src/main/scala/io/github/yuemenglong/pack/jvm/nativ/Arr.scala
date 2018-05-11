package io.github.yuemenglong.pack.jvm.nativ

import io.github.yuemenglong.pack.jvm.struct.ClassFile

import scala.reflect.ClassTag

/**
  * Created by <yuemenglong@126.com> on 2018/2/23.
  */
trait Arr extends Ref {
  def store(idx: Int, value: Any): Unit

  def load(idx: Int): Any

  val size: Int

  val dim: Int
}

class ArrI[T: ClassTag](val size: Int, init: Array[T] = null) extends Arr {
  def this(init: Array[T]) = this(init.length, init)

  require(clazz != classOf[Obj])

  val array = init match {
    case null => new Array[T](size)
    case _ => init
  }

  def clazz = implicitly[ClassTag[T]].runtimeClass

  def store(idx: Int, value: Any): Unit = {
    array(idx) = value.asInstanceOf[T]
  }

  def load(idx: Int): Any = array(idx)

  override def toString = {
    s"Arr[${clazz}]@[${id}]"
  }

  override val dim = 1
}

class ArrA(val cf: ClassFile, val size: Int, init: Array[Obj] = null) extends Arr {

  val array = init match {
    case null => new Array[Obj](size)
    case _ => init
  }

  def store(idx: Int, value: Any): Unit = {
    array(idx) = value.asInstanceOf[Obj]
  }

  def load(idx: Int): Any = array(idx)

  override def toString = {
    s"Arr[${cf.name}]@[${id}]"
  }

  override val dim = 1
}
