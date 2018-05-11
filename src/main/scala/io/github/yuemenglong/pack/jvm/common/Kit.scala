package io.github.yuemenglong.pack.jvm.common

import java.lang.reflect.{Field, Method}

import io.github.yuemenglong.pack.jvm.rt.{ThreadCtx, Vm}
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, FieldInfo, MethodInfo}
import io.github.yuemenglong.pack.jvm.rt.{ThreadCtx, Vm}
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, FieldInfo, MethodInfo}

/**
  * Created by <yuemenglong@126.com> on 2018/2/19.
  */
object Kit {
  def makeVariableTable(ctx: ThreadCtx, n: Int): Map[Int, Any] = {
    (1 to n).map(_ => ctx.pop()).reverse.foldLeft(Map[Int, Any]()) { case (map, v) =>
      map.keys.isEmpty match {
        case true => map + (0 -> v)
        case false =>
          val max = map.keys.max
          map(max).isInstanceOf[Double] || map(max).isInstanceOf[Long] match {
            case true => map + (max + 2 -> v)
            case false => map + (max + 1 -> v)
          }
      }
    }
  }

  def debug(s: Any*): Unit = {
    println(s"${s.mkString(", ")}")
  }

  def findMethod(cf: ClassFile, name: String, descriptor: String): MethodInfo = {
    var cur = cf
    Stream.continually({
      val ret = cur.method(name, descriptor)
      if (ret == null) {
        cur = Vm.rt.superClazz(cur)
      }
      (cur, ret)
    }).takeWhile(_._1 != null).find(_._2 != null) match {
      case Some(m) => m._2
      case None => null
    }
  }

  def getCfFields(cf: ClassFile): Array[FieldInfo] = {
    val p = cf.sup match {
      case null => Array[FieldInfo]()
      case _ => getCfFields(Vm.rt.load(cf.sup))
    }
    cf.fields ++ p
  }

  def defaultFieldValue(f: FieldInfo): Any = {
    f.descriptor match {
      case "B" => 0.toByte
      case "C" => 0.toChar
      case "D" => 0.toDouble
      case "F" => 0.toFloat
      case "I" => 0
      case "J" => 0.toLong
      case "S" => 0.toShort
      case "Z" => false
      case _ => null
    }
  }

  def getAncestor(cf: ClassFile): Array[ClassFile] = {
    val sup = cf.sup match {
      case null => Array[ClassFile]()
      case _ => Array(Vm.rt.load(cf.sup))
    }
    val layer = sup ++ cf.impls.map(Vm.rt.load)
    layer ++ layer.flatMap(getAncestor)
  }

  def getDeclaredFields(clazz: Class[_]): Array[Field] = {
    val parent = clazz.getSuperclass
    if (parent != null) {
      getDeclaredFields(parent) ++ clazz.getDeclaredFields
    } else {
      clazz.getDeclaredFields
    }
  }

  def getDeclaredMethods(clazz: Class[_]): Array[Method] = {
    val parent = clazz.getSuperclass
    if (parent != null) {
      getDeclaredMethods(parent) ++ clazz.getDeclaredMethods
    } else {
      clazz.getDeclaredMethods
    }
  }

  def params(descriptor: String): Array[String] = {
    val contentRe = """\((.*)\).*""".r
    val content = descriptor match {
      case contentRe(c) => c
    }
    val re = """((\[?[BCDFIJSZ])|(\[?L.+?;))""".r
    re.findAllMatchIn(content).map(_.group(0)).toArray
  }
}
