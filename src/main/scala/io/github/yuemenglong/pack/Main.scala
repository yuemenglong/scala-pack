package io.github.yuemenglong.pack

import java.io._
import java.nio.file.Files
import java.util.jar.JarFile
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.struct.ClassFile
import io.github.yuemenglong.pack.kit.{Args, Kit}
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer

trait GetPkg {
  val clazz: String
  val pkg: String = clazz.split("/").dropRight(1).mkString("/")
}

case class JarItem(clazz: String, jar: String) extends GetPkg

case class ClazzItem(file: String, clazz: String) extends GetPkg

case class PackItem(file: String, clazz: String, jar: String) extends GetPkg

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
case class Lib(libDir: String, jarFile: String, deep: Int) {
  def scan(file: File, deep: Int = 1): Array[File] = {
    if (file.isFile) {
      Array(file)
    } else if (file.isDirectory && deep > 0) {
      file.listFiles().flatMap(scan(_, deep - 1))
    } else {
      Array()
    }
  }

  def exec(): Array[JarItem] = {
    val files = if (jarFile != null) {
      Array(new File(jarFile))
    } else {
      val dir = new File(libDir)
      if (!dir.isDirectory) {
        throw new Exception(s"${libDir} Not A Dir")
      }
      scan(dir, deep)
    }
    files.filter(_.getName.endsWith(".jar")).flatMap(file => {
      println(s"Scan Lib File: ${file}")
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
      println(s"Copy ${new File(c.file).getName}")
    })
  }
}

// PackItems
case class JoinPackItems(clazzs: Array[ClazzItem], jars: Array[JarItem]) {
  val pkgToJar: Map[String, Set[String]] = jars.flatMap(j => {
    val items = j.pkg.split("/")
    (1 to items.length).map(i => {
      (items.take(i).mkString("/"), j.jar)
    })
  }).groupBy(_._1).mapValues(_.map(_._2).toSet)

  val clazzToJar: Map[String, String] = jars.map(o => (o.clazz, o.jar)).toMap

  val outerClassToJar: Map[String, String] = jars.flatMap(o => o.clazz.contains("$") match {
    case false => Array[(String, String)]()
    case true =>
      var pre = ""
      o.clazz.replace(".class", "").split("\\$").map(seg => {
        pre = pre match {
          case "" => seg
          case _ => pre + "$" + seg
        }
        (pre, o.jar)
      })
  }).toMap

  private def findJarByOuter(clazz: String): Option[String] = {
    if (!clazz.contains("$")) {
      return None
    }
    val segs = clazz.replace(".class", "").split("\\$")
    (1 to segs.length).reverse.find(i => {
      val k = segs.take(i).mkString("$")
      outerClassToJar.contains(k)
    }).map(i => outerClassToJar(segs.take(i).mkString("$")))
  }

  private def findJarByPkg(pkg: String): Option[String] = {
    val items = pkg.split("/")
    (1 to items.length).reverse.find(i => {
      pkgToJar.get(items.take(i).mkString("/")) match {
        case Some(set) => set.size match {
          case 1 => true
          case _ => false
        }
        case None => false
      }
    }).map(i => pkgToJar(items.take(i).mkString("/")).toIndexedSeq(0))
  }

  private def findJar(c: ClazzItem): Option[String] = {
    findJarByOuter(c.clazz).orElse(findJarByPkg(c.pkg))
  }

  def exec(): Array[PackItem] = {
    clazzs.map(c => {
      clazzToJar.get(c.clazz) match {
        // 原本有的情况下，直接替换
        case Some(j) => PackItem(c.file, c.clazz, j)
        // 原来没有这个文件，找到对应的jar文件
        case None => findJar(c) match {
          case Some(j) => PackItem(c.file, c.clazz, j)
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
case class Pack(items: Array[PackItem], workDir: String) {

  private def updateJar(jarPath: String, files: Array[PackItem]): Unit = {
    println(s"Update Jar [${jarPath}]")
    val fileMap = files.map(f => (f.clazz, f.file)).toMap
    val jarFile = new File(jarPath)
    val tmpFile = File.createTempFile(jarFile.getName, null, new File(workDir))
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
      println(s"Pack [${item.clazz}]")
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
    println(s"WorkDir: ${workDir}")
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

object Main {
  def main(args: Array[String]): Unit = {
    Args.option("m", hasArg = true, "Mode : P[ick] | W[atch]")
    Args.parse(args)

    val mode = Args.getOptionValue("m").toLowerCase()
    mode match {
      case "p" =>
        Args.option("lib", true, "To Update Jar Lib Dir", null)
        Args.option("jar", true, "Directly Update Jar", null)
        Args.option("dir", true, "Class File Dir")
        Args.option("rm", false, "Need Remove Class File")
        Args.option("deep", true, "Scan Dir Deep", "1")
        Args.parse(args)
        val libDir = Args.getOptionAsPath("lib")
        val jarFile = Args.getOptionAsPath("jar")
        if (libDir == null && jarFile == null) {
          Args.printUsage()
          System.exit(0)
        }
        val clazzDir = Args.getOptionAsPath("dir")
        val rm = Args.hasOption("rm")
        val deep = Args.getOptionValue("deep").toInt
        println("Pick Mode", libDir, clazzDir, rm)
        val jars = Lib(libDir, jarFile, deep).exec()
        val clazzs = Clazz(clazzDir).exec()
        val packs = JoinPackItems(clazzs, jars).exec()
        Backup(packs).exec()
        Pack(packs, clazzDir).exec()
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
          case _ => Lib(lib, null, 1).exec()
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
              Backup(packs).exec()
              Pack(packs, dir).exec()
          }
        }
      case _ => Args.printUsage()
    }
  }
}
