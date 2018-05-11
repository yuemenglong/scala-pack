package io.github.yuemenglong.pack.jvm.nativ

import io.github.yuemenglong.pack.jvm.common.UnreachableException

import scala.reflect.ClassTag

/**
  * Created by <yuemenglong@126.com> on 2018/2/22.
  */

object Num {
  def improve(a: Any, b: Any): (Any, Any) = {
    if (a.getClass == b.getClass) {
      return (a, b)
    }
    (a, b) match {
      case (a: Float, b: Double) => (a.toDouble, b.toDouble)
      case (a: Double, b: Float) => (a.toDouble, b.toDouble)
      case (a: Byte, b: Short) => (a.toInt, b.toInt)
      case (a: Byte, b: Char) => (a.toInt, b.toInt)
      case (a: Byte, b: Int) => (a.toInt, b.toInt)
      case (a: Byte, b: Long) => (a.toLong, b.toLong)
      case (a: Short, b: Byte) => (a.toInt, b.toInt)
      case (a: Short, b: Char) => (a.toInt, b.toInt)
      case (a: Short, b: Int) => (a.toInt, b.toInt)
      case (a: Short, b: Long) => (a.toLong, b.toLong)
      case (a: Int, b: Byte) => (a.toInt, b.toInt)
      case (a: Int, b: Char) => (a.toInt, b.toInt)
      case (a: Int, b: Short) => (a.toInt, b.toInt)
      case (a: Int, b: Long) => (a.toLong, b.toLong)
      case (a: Long, b: Byte) => (a.toLong, b.toLong)
      case (a: Long, b: Char) => (a.toLong, b.toLong)
      case (a: Long, b: Short) => (a.toLong, b.toLong)
      case (a: Long, b: Int) => (a.toLong, b.toLong)
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def toInt(a: Any): Int = {
    a match {
      case a: Byte => a.toInt
      case a: Short => a.toInt
      case a: Int => a.toInt
      case _ => throw new UnreachableException
    }
  }

  def add(a: Any, b: Any): Any = {
    improve(a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a + b
      case (a: Short, b: Short) => a + b
      case (a: Int, b: Int) => a + b
      case (a: Long, b: Long) => a + b
      case (a: Float, b: Float) => a + b
      case (a: Double, b: Double) => a + b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def sub(a: Any, b: Any): Any = {
    improve(a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a - b
      case (a: Short, b: Short) => a - b
      case (a: Int, b: Int) => a - b
      case (a: Long, b: Long) => a - b
      case (a: Float, b: Float) => a - b
      case (a: Double, b: Double) => a - b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def mul(a: Any, b: Any): Any = {
    improve(a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a * b
      case (a: Short, b: Short) => a * b
      case (a: Int, b: Int) => a * b
      case (a: Long, b: Long) => a * b
      case (a: Float, b: Float) => a * b
      case (a: Double, b: Double) => a * b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def div(a: Any, b: Any): Any = {
    improve(a, b) match {
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a / b
      case (a: Short, b: Short) => a / b
      case (a: Int, b: Int) => a / b
      case (a: Long, b: Long) => a / b
      case (a: Float, b: Float) => a / b
      case (a: Double, b: Double) => a / b
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def rem(a: Any, b: Any): Any = {
    improve(a, b) match {
      case (_, 0) => Float.NaN
      case (Float.NaN, _) => Float.NaN
      case (Double.NaN, _) => Double.NaN
      case (_, Float.NaN) => Float.NaN
      case (_, Double.NaN) => Double.NaN
      case (a: Byte, b: Byte) => a - ((a / b) * b)
      case (a: Short, b: Short) => a - ((a / b) * b)
      case (a: Int, b: Int) => a - ((a / b) * b)
      case (a: Long, b: Long) => a - ((a / b) * b)
      case (a: Float, b: Float) => a - (Math.round(a / b) * b)
      case (a: Double, b: Double) => a - (Math.round(a / b) * b)
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def cmp(a: Any, b: Any, nan: Int = -1): Int = {
    improve(a, b) match {
      case (Float.NaN, _) => nan
      case (Double.NaN, _) => nan
      case (_, Float.NaN) => nan
      case (_, Double.NaN) => nan
      case (a: Byte, b: Byte) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Short, b: Short) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Int, b: Int) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Long, b: Long) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Float, b: Float) => if (a < b) -1 else if (a > b) 1 else 0
      case (a: Double, b: Double) => if (a < b) -1 else if (a > b) 1 else 0
      case _ => throw new RuntimeException("Unreachable")
    }
  }

  def neg(a: Any): Any = {
    a match {
      case Float.NaN => a
      case Double.NaN => a
      case a: Byte => -a
      case a: Short => -a
      case a: Int => -a
      case a: Long => -a
      case a: Float => -a
      case a: Double => -a
    }
  }

  def shl(a: Any, b: Any): Any = {
    (a, toInt(b)) match {
      case (a: Int, b: Int) => a << b
      case (a: Long, b: Int) => a << b
      case _ => throw new UnreachableException
    }
  }

  def shr(a: Any, b: Any): Any = {
    (a, toInt(b)) match {
      case (a: Int, b: Int) => a >> b
      case (a: Long, b: Int) => a >> b
      case _ => throw new UnreachableException
    }
  }

  def ushr(a: Any, b: Any): Any = {
    (a, toInt(b)) match {
      case (a: Int, b: Int) => a >>> b
      case (a: Long, b: Int) => a >>> b
      case _ => throw new UnreachableException
    }
  }

  def and(a: Any, b: Any): Any = {
    (a, b) match {
      case (a: Int, b: Int) => a & b
      case (a: Long, b: Long) => a & b
      case _ => throw new UnreachableException
    }
  }

  def or(a: Any, b: Any): Any = {
    (a, b) match {
      case (a: Int, b: Int) => a | b
      case (a: Long, b: Long) => a | b
      case _ => throw new UnreachableException
    }
  }

  def xor(a: Any, b: Any): Any = {
    (a, b) match {
      case (a: Int, b: Int) => a ^ b
      case (a: Long, b: Long) => a ^ b
      case _ => throw new UnreachableException
    }
  }

  def convert(v: Any, clazz: Class[_]): Any = {
    if (clazz == classOf[Byte]) {
      v.toString.toByte
    } else if (clazz == classOf[Char]) {
      v.toString.toInt.toChar
    } else if (clazz == classOf[Short]) {
      v.toString.toShort
    } else if (clazz == classOf[Int]) {
      v.toString.toInt
    } else if (clazz == classOf[Long]) {
      v.toString.toLong
    } else if (clazz == classOf[Float]) {
      v.toString.toFloat
    } else if (clazz == classOf[Double]) {
      v.toString.toDouble
    } else {
      ???
    }
  }
}
