package io.github.yuemenglong.pack.jvm.rt

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.Paths
import java.util.jar.JarFile

import io.github.yuemenglong.pack.jvm.common.{Kit, StreamReader, UnreachableException}
import io.github.yuemenglong.pack.jvm.nativ.{Obj, Ref}
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}
import io.github.yuemenglong.pack.jvm.common.{Kit, StreamReader}
import io.github.yuemenglong.pack.jvm.nativ.{Obj, Ref}
import io.github.yuemenglong.pack.jvm.struct.{ClassFile, MethodInfo}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by <yuemenglong@126.com> on 2018/2/12.
  */


class RuntimeCtx {
  private var heap: Map[Int, Ref] = Map()
  private var clazzLoaderMap: Map[String, InputStream] = Map()
  private var clazzMetaMap: Map[String, ClassFile] = Map()
  private var clazzMap: Map[String, RuntimeCls] = Map()
  private var threads: ArrayBuffer[ThreadCtx] = new ArrayBuffer[ThreadCtx]()

  def clazzpath(root: String): Unit = {
    if (root.endsWith(".jar")) {
      val jf = new JarFile(root)
      val es = jf.entries()
      Stream.continually({
        es.hasMoreElements match {
          case true => es.nextElement()
          case false => null
        }
      }).takeWhile(_ != null).foreach(je => {
        if (!je.isDirectory && je.getName.endsWith(".class")) {
          clazzLoaderMap += (je.getName.replace(".class", "") -> jf.getInputStream(je))
        }
      })
    } else {
      val rootAbs = Paths.get(root).toAbsolutePath

      def go(file: File): Unit = {
        if (file.isDirectory) {
          file.listFiles().foreach(go)
        } else if (file.getName.endsWith(".class")) {
          val rel = rootAbs.relativize(Paths.get(file.getAbsolutePath)).toString.replaceAll("\\\\", "/")
          clazzLoaderMap += (rel.replace(".class", "") -> new FileInputStream(file))
        }
      }

      go(new File(root))
    }
  }

  def createObject(cf: ClassFile): Obj = {
    val obj = new Obj(cf)
    heap += (obj.id -> obj)
    obj
  }

  def createThread(method: MethodInfo, vt: Map[Int, Any] = Map()): ThreadCtx = {
    if (!clazzMetaMap.contains(method.cf.name)) {
      clazzMetaMap += (method.cf.name -> method.cf)
    }
    threads += new ThreadCtx(method, this, vt)
    Kit.debug("[CreateThread]")
    threads.last
  }

  def finishThread(t: ThreadCtx): Unit = {
    Kit.debug("[FinishThread]")
    threads -= t
  }

  def superClazz(cf: ClassFile): ClassFile = {
    cf.sup match {
      case null => null
      case _ => load(cf.sup)
    }
  }

  def load(path: String): ClassFile = {
    if (clazzMetaMap.contains(path)) {
      clazzMetaMap(path)
    } else if (clazzLoaderMap.contains(path)) {
      load(clazzLoaderMap(path))
    } else
      throw new RuntimeException(s"Unknown Class [${path}]")
  }

  def load(is: InputStream): ClassFile = {
    val reader = new StreamReader(is)
    val cf = new ClassFile(reader)
    Kit.debug(s"[Load] ${cf.name}")
    clazzMetaMap += (cf.name -> cf)
    clazzMap += (cf.name -> new RuntimeClsT(cf))
    val clinit = cf.method("<clinit>")
    if (clinit != null) {
      Vm.run(clinit)
    }
    Kit.debug(s"[Load] ${cf.name} SUCC")
    cf
  }

  def getStatic(cf: ClassFile, key: String): Any = clazzMap(cf.name).getStatic(key)

  def putStatic(cf: ClassFile, key: String, value: Any): Unit = clazzMap(cf.name).putStatic(key, value)

  def callStatic(ctx: ThreadCtx, cf: ClassFile, name: String, descriptor: String): Unit = {
    val m = NativeCall.staticNatives(cf.name, name, descriptor)
    m(ctx)
  }

  def callVirtual(ctx: ThreadCtx, cf: ClassFile, name: String, descriptor: String): Unit = {
    val m = NativeCall.virtualNatives(cf.name, name, descriptor)
    m(ctx)
  }

  def getClass(cf: ClassFile): Obj = {
    clazzMap(cf.name).clazz
  }

  def getClass(cf: ClassFile, dim: Int): Obj = {
    val name = (1 to dim).map(_ => "[").mkString("") + cf.name + ";"
    clazzMap.get(name) match {
      case Some(r) => r.clazz
      case None =>
        val rtc = new RuntimeClsAT(cf, dim)
        clazzMap += (name -> rtc)
        rtc.clazz
    }
  }

  def getClass(t: String, dim: Int): Obj = {
    val name = (1 to dim).map(_ => "[").mkString("") + t
    clazzMap.get(name) match {
      case Some(r) => r.clazz
      case None =>
        val rtc = new RuntimeClsA(t, dim)
        clazzMap += (name -> rtc)
        rtc.clazz
    }
  }
}








