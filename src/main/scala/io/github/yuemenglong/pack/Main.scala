package io.github.yuemenglong.pack

import java.io._
import java.nio.file.Files
import java.util.jar.JarFile
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.struct.ClassFile
import io.github.yuemenglong.pack.kit.{Args, Kit}
import io.github.yuemenglong.pack.watcher.Main.scan
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer


case class JarItem(clazz: String, jar: String)

case class ClazzItem(file: String, clazz: String)

case class PackItem(file: String, clazz: String, jar: String)

object ClazzItem {
  private def getFullNameFromClassFile(is: InputStream): String = {
    val sr = new StreamReader(is)
    val cf = new ClassFile(sr)
    is.close()
    cf.name + ".class"
  }

  def apply(f: File): ClazzItem = {
    val file = f.getAbsolutePath
    val clazz = getFullNameFromClassFile(new FileInputStream(f))
    ClazzItem(file, clazz)
  }
}

// Jars
case class Lib(libDir: String) {
  def exec(): Array[JarItem] = {
    val dir = new File(libDir)
    if (!dir.isDirectory) {
      throw new Exception(s"${libDir} Not A Dir")
    }
    dir.listFiles().filter(_.getName.endsWith(".jar")).flatMap(file => {
      val jarFile = new JarFile(file)
      val entries = jarFile.entries()
      val ret = Stream.continually({
        entries.hasMoreElements match {
          case true => entries.nextElement()
          case false => null
        }
      }).takeWhile(_ != null).filter(_.getName.endsWith(".class"))
        .map(f => JarItem(f.getName, file.getAbsolutePath)).toArray
      jarFile.close()
      ret
    })
  }
}

// Clazzs
case class Clazz(clazzDir: String) {
  private def getFullNameFromClassFile(is: InputStream): String = {
    val sr = new StreamReader(is)
    val cf = new ClassFile(sr)
    is.close()
    cf.name + ".class"
  }

  def exec(): Array[ClazzItem] = {
    val dir = new File(clazzDir)
    if (!dir.isDirectory) {
      throw new Exception(s"${clazzDir} Not A Dir")
    }

    dir.listFiles().filter(_.getName.endsWith(".class")).map(f => ClazzItem(f))
  }
}

// Watch Which Modified
case class Watch(watchDir: String) {
  private var cache = {
    val ab = new ArrayBuffer[(String, Long)]
    scan(watchDir, (f: File) => {
      ab += ((f.getAbsolutePath, f.lastModified()))
    })
    ab.toMap
  }
  println(s"Start Watch, Files: ${cache.size}")

  private def scan(dir: File, fn: File => Any): Unit = {
    dir.isDirectory match {
      case true => dir.listFiles().foreach(f => {
        f.getName.startsWith(".") match {
          case true =>
          case false => f.isDirectory match {
            case true => scan(f, fn)
            case false => f.getName.endsWith(".class") match {
              case false =>
              case true => fn(f)
            }
          }
        }
      })
      case false => throw new Exception(s"${dir.getAbsolutePath} Is Not Dir")
    }
  }

  private def scan(path: String, fn: File => Any): Unit = scan(new File(path), fn)

  def exec(): Array[ClazzItem] = {
    val ab = new ArrayBuffer[ClazzItem]()
    val start = System.currentTimeMillis()
    scan(watchDir, (f: File) => {
      cache.get(f.getAbsolutePath) match {
        case None =>
          ab += ClazzItem(f)
          cache += (f.getAbsolutePath -> f.lastModified())
        case Some(t) => if (f.lastModified() > t) {
          ab += ClazzItem(f)
          cache += (f.getAbsolutePath -> f.lastModified())
        }
      }
    })
    val end = System.currentTimeMillis()
    println(s"Watch Once Use: ${end - start}, Files: ${cache.size}")
    ab.toArray
  }
}

// Copy After Watch
case class Copy(clazzs: Array[ClazzItem], to: String) {
  def exec(): Unit = {
    clazzs.foreach(c => {
      FileUtils.copyFileToDirectory(new File(c.file), new File(to))
      println(s"Copy ${c.file} To ${to}")
    })
  }
}

// PackItems
case class JoinPackItems(clazzs: Array[ClazzItem], jars: Array[JarItem]) {
  private def outerClazz(clazz: String): String = {
    val items = clazz.split("/")
    items(items.length - 1) = items(items.length - 1).split("\\$")(0) + ".class"
    val ret = items.mkString("/")
    println(s"[${clazz}] With Outer [${ret}]")
    ret
  }

  def exec(): Array[PackItem] = {
    val jarMap = jars.map(o => (o.clazz, o)).toMap
    clazzs.map(c => {
      jarMap.get(c.clazz) match {
        case Some(j) => PackItem(c.file, j.clazz, j.jar)
        case None => jarMap.get(outerClazz(c.clazz)) match {
          case Some(j) => PackItem(c.file, j.clazz, j.jar)
          case None => throw new Exception(s"No Class [${c.clazz}] In Libs")
        }
      }
    })
  }
}

case class Backup(items: Array[PackItem]) {
  def exec(): Unit = {
    items.map(_.jar).toSet[String].foreach(path => {
      val backPath = s"${path}.bak"
      if (!new File(backPath).exists()) {
        println(s"[${path}] BackUp")
        FileUtils.copyFile(new File(path), new File(backPath))
        //        exec(s"cp ${path} ${backPath}")
      } else {
        println(s"[${backPath}] Exists")
      }
    })
  }
}

// Do Pack
case class Pack(items: Array[PackItem]) {
  private val workDir: File = Kit.getThisJarFile.getParentFile

  private def updateJar(jarPath: String, files: Array[PackItem]): Unit = {
    println(s"Update Jar [${jarPath}]")
    val fileMap = files.map(f => (f.clazz, f.file)).toMap
    val jarFile = new File(jarPath)
    val tmpFile = File.createTempFile(jarFile.getName, null, workDir)
    tmpFile.delete()
    Files.move(jarFile.toPath, tmpFile.toPath)

    val zis = new ZipInputStream(new FileInputStream(tmpFile))
    val zos = new ZipOutputStream(new FileOutputStream(jarFile))
    // 1. copy non update
    Stream.continually(zis.getNextEntry).takeWhile(_ != null).foreach(f => {
      fileMap.contains(f.getName) match {
        case true => println(s"[${f.getName}] Need Update")
        case false =>
          zos.putNextEntry(f)
          Kit.pipe(zis, zos)
      }
    })
    zis.close()

    // 2. copy update
    files.foreach(item => {
      println(s"Update [${item.clazz}]")
      val is = new FileInputStream(item.file)
      zos.putNextEntry(new ZipEntry(item.clazz))
      Kit.pipe(is, zos)
      is.close()
      zos.closeEntry()
    })
    zos.close()

    tmpFile.delete()
  }

  def exec(): Unit = {
    println(s"WorkDir: ${workDir.getAbsolutePath}")
    items.groupBy(_.jar).foreach { case (jar, arr) =>
      updateJar(jar, arr)
    }
  }
}

// Remove Clazzs
case class Clean(clazzs: Array[ClazzItem]) {
  def exec(): Unit = {
    clazzs.foreach(c => FileUtils.forceDelete(new File(c.file)))
  }
}

//
//class Pack2(libDir: String, clazzDir: String, rm: Boolean = false) {
//  // class -> jar
//  val libMap: Map[String, String] = scanLibJar(libDir)
//  // file, package, jar
//  val packItems: Array[PackItem] = scanLocalClass(clazzDir)
//
//  val curDir: String = System.getProperty("user.dir")
//
//  def outerClazz(clazz: String): String = {
//    val items = clazz.split("/")
//    items(items.length - 1) = items(items.length - 1).split("\\$")(0) + ".class"
//    val ret = items.mkString("/")
//    println(s"[${clazz}] With Outer [${ret}]")
//    ret
//  }
//
//  // className=>jarPath
//  def scanLibJar(libDir: String): Map[String, String] = {
//    val dir = new File(libDir)
//    if (!dir.isDirectory) {
//      throw new Exception(s"${libDir} Not A Dir")
//    }
//    dir.listFiles().filter(_.getName.endsWith(".jar")).flatMap(file => {
//      val jarFile = new JarFile(file)
//      try {
//        val entries = jarFile.entries()
//        Stream.continually({
//          entries.hasMoreElements match {
//            case true => entries.nextElement()
//            case false => null
//          }
//        }).takeWhile(_ != null).filter(_.getName.endsWith(".class"))
//          .map(f => (f.getName, file.getAbsolutePath)).toArray[(String, String)]
//      } finally {
//        jarFile.close()
//      }
//    }).toMap
//  }
//
//  // fileName=>className
//  def scanLocalClass(clazzDir: String): Array[PackItem] = {
//    val dir = new File(clazzDir)
//    if (!dir.isDirectory) {
//      throw new Exception(s"${clazzDir} Not A Dir")
//    }
//
//    def getFullNameFromClassFile(is: InputStream): String = {
//      val sr = new StreamReader(is)
//      val cf = new ClassFile(sr)
//      is.close()
//      cf.name + ".class"
//    }
//
//    dir.listFiles().filter(_.getName.endsWith(".class")).map(f => {
//      val file = f.getAbsolutePath
//      val clazz = getFullNameFromClassFile(new FileInputStream(f))
//      val pkg = clazz.split("/").dropRight(1).mkString("/")
//      val jar = libMap.contains(clazz) match {
//        case true => libMap(clazz)
//        case false => libMap(outerClazz(clazz))
//      }
//      PackItem(file, clazz, jar)
//    })
//  }
//
//  def pipe(is: InputStream, os: OutputStream): Unit = {
//    val buffer = new Array[Byte](4 * 1024)
//    Stream.continually(is.read(buffer)).takeWhile(_ >= 0).foreach(read => {
//      os.write(buffer, 0, read)
//    })
//  }
//
//  def checkBackup(): Unit = {
//    packItems.map(_.jar).toSet[String].foreach(path => {
//      val backPath = s"${path}.bak"
//      if (!new File(backPath).exists()) {
//        println(s"[${path}] BackUp")
//        FileUtils.copyFile(new File(path), new File(backPath))
//        //        exec(s"cp ${path} ${backPath}")
//      } else {
//        println(s"[${backPath}] Exists")
//      }
//    })
//  }
//
//  def updateJar(jarPath: String, files: Array[PackItem]): Unit = {
//    println(s"Update Jar [${jarPath}]")
//    val fileMap = files.map(f => (f.clazz, f.file)).toMap
//    val jarFile = new File(jarPath)
//    val tmpFile = File.createTempFile(jarFile.getName, null, new File(curDir))
//    tmpFile.delete()
//    Files.move(jarFile.toPath, tmpFile.toPath)
//
//    val zis = new ZipInputStream(new FileInputStream(tmpFile))
//    val zos = new ZipOutputStream(new FileOutputStream(jarFile))
//    // 1. copy non update
//    Stream.continually(zis.getNextEntry).takeWhile(_ != null).foreach(f => {
//      fileMap.contains(f.getName) match {
//        case true => println(s"[${f.getName}] Need Update")
//        case false =>
//          zos.putNextEntry(f)
//          pipe(zis, zos)
//      }
//    })
//    zis.close()
//
//    // 2. copy update
//    files.foreach(item => {
//      println(s"Update [${item.clazz}]")
//      val is = new FileInputStream(item.file)
//      zos.putNextEntry(new ZipEntry(item.clazz))
//      pipe(is, zos)
//      is.close()
//      zos.closeEntry()
//    })
//    zos.close()
//
//    // 3. clear if rm option
//    if (rm) {
//      files.foreach(f => FileUtils.forceDelete(new File(f.file)))
//    }
//
//    tmpFile.delete()
//  }
//
//  def update(): Unit = {
//    println(s"Current Dir: [${curDir}]")
//    packItems.groupBy(_.jar).foreach { case (jar, arr) =>
//      updateJar(jar, arr)
//    }
//  }
//
//  def pack(): Unit = {
//    checkBackup()
//    update()
//  }
//}

object Main {
  def main(args: Array[String]): Unit = {
    Args.option("m", hasArg = true, "Mode : P[ick] | W[atch]")
    Args.parse(args)

    val mode = Args.getOptionValue("m").toLowerCase()
    mode match {
      case "p" =>
        Args.option("lib", true, "To Update Jar Lib Dir")
        Args.option("dir", true, "Class File Dir")
        Args.option("rm", false, "Need Remove Class File")
        Args.parse(args)
        val libDir = Args.getOptionAsPath("lib")
        val clazzDir = Args.getOptionAsPath("dir")
        val rm = Args.hasOption("rm")
        println("Pick Mode", libDir, clazzDir, rm)
        val jars = Lib(libDir).exec()
        val clazzs = Clazz(clazzDir).exec()
        val packs = JoinPackItems(clazzs, jars).exec()
        Backup(packs)
        Pack(packs).exec()
        if (rm) {
          Clean(clazzs).exec()
        }
      case "w" =>
        println("Watch Mode")
        Args.option("lib", true, "To Update Jar Lib Dir", null)
        Args.option("dir", true, "Watch Dir")
        Args.option("to", true, "Copy To Dir", null)
        Args.option("t", true, "Loop Timeout", "3000")
        Args.parse(args)
        val lib = Args.getOptionAsPath("lib")
        val dir = Args.getOptionAsPath("dir")
        val to = Args.getOptionAsPath("to")
        if ((lib == null && to == null) || (lib != null && to != null)) {
          println(3)
          Args.printUsage("Lib/To Must Has Exactly One")
        }
        val jars: Array[JarItem] = lib match {
          case null => Array()
          case _ => Lib(lib).exec()
        }
        val timeout = Args.getOptionValue("t").toInt
        val watch = Watch(dir)
        while (true) {
          Thread.sleep(timeout)
          val clazzs = watch.exec()
          lib match {
            case null =>
              Copy(clazzs, to).exec()
            case _ =>
              val packs = JoinPackItems(clazzs, jars).exec()
              Backup(packs)
              Pack(packs).exec()
          }
        }
      case _ => Args.printUsage()
    }
  }

  //
  //
  //    Args.option("w", false, "Watcher Mode")
  //
  //    if (args.indexOf("-w") >= 0) {
  //      watcher.Main.main(args)
  //      return
  //    }
  //
  //    Args.option("lib", true, "To Update Jar Lib Dir", null)
  //    Args.option("dir", true, "Class File Dir")
  //    Args.option("rm", false, "Need Remove Class File")
  //
  //    Args.parse(args)
  //
  //    val libDir = Args.getOptionAsPath("lib")
  //    val clazzDir = Args.getOptionAsPath("dir")
  //    val rm = Args.hasOption("rm")
  //
  //    println(s"Lib: ${libDir}")
  //    println(s"Dir: ${clazzDir}")
  //    println(s"Rm: ${rm}")
  //
  //    //    "D:\\workspace\\scala\\scala-test\\target\\classes\\test"
  //    val p = new Pack2(libDir, clazzDir, rm)
  //    p.pack()
  //  }
}
