package io.github.yuemenglong.pack

import java.io._
import java.nio.file.Files
import java.util.jar.JarFile
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.struct.ClassFile
import io.github.yuemenglong.pack.kit.Args
import org.apache.commons.io.FileUtils

case class PackItem(file: String, clazz: String, pkg: String, jar: String)

class Pack(libDir: String, clazzDir: String, rm: Boolean = false) {
  // class -> jar
  val libMap: Map[String, String] = scanLibJar(libDir)
  // file, package, jar
  val packItems: Array[PackItem] = scanLocalClass(clazzDir)

  val curDir: String = System.getProperty("user.dir")

  def outerClazz(clazz: String): String = {
    val items = clazz.split("/")
    items(items.length - 1) = items(items.length - 1).split("\\$")(0) + ".class"
    val ret = items.mkString("/")
    println(s"[${clazz}] With Outer [${ret}]")
    ret
  }

  // className=>jarPath
  def scanLibJar(libDir: String): Map[String, String] = {
    val dir = new File(libDir)
    if (!dir.isDirectory) {
      throw new Exception(s"${libDir} Not A Dir")
    }
    dir.listFiles().filter(_.getName.endsWith(".jar")).flatMap(file => {
      val jarFile = new JarFile(file)
      try {
        val entries = jarFile.entries()
        Stream.continually({
          entries.hasMoreElements match {
            case true => entries.nextElement()
            case false => null
          }
        }).takeWhile(_ != null).filter(_.getName.endsWith(".class"))
          .map(f => (f.getName, file.getAbsolutePath)).toArray[(String, String)]
      } finally {
        jarFile.close()
      }
    }).toMap
  }

  // fileName=>className
  def scanLocalClass(clazzDir: String): Array[PackItem] = {
    val dir = new File(clazzDir)
    if (!dir.isDirectory) {
      throw new Exception(s"${clazzDir} Not A Dir")
    }

    def getFullNameFromClassFile(is: InputStream): String = {
      val sr = new StreamReader(is)
      val cf = new ClassFile(sr)
      is.close()
      cf.name + ".class"
    }

    dir.listFiles().filter(_.getName.endsWith(".class")).map(f => {
      val file = f.getAbsolutePath
      val clazz = getFullNameFromClassFile(new FileInputStream(f))
      val pkg = clazz.split("/").dropRight(1).mkString("/")
      val jar = libMap.contains(clazz) match {
        case true => libMap(clazz)
        case false => libMap(outerClazz(clazz))
      }
      PackItem(file, clazz, pkg, jar)
    })
  }

  def pipe(is: InputStream, os: OutputStream): Unit = {
    val buffer = new Array[Byte](4 * 1024)
    Stream.continually(is.read(buffer)).takeWhile(_ >= 0).foreach(read => {
      os.write(buffer, 0, read)
    })
  }

  def checkBackup(): Unit = {
    packItems.map(_.jar).toSet[String].foreach(path => {
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

  def updateJar(jarPath: String, files: Array[PackItem]): Unit = {
    println(s"Update Jar [${jarPath}]")
    val fileMap = files.map(f => (f.clazz, f.file)).toMap
    val jarFile = new File(jarPath)
    val tmpFile = File.createTempFile(jarFile.getName, null, new File(curDir))
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
          pipe(zis, zos)
      }
    })
    zis.close()

    // 2. copy update
    files.foreach(item => {
      println(s"Update [${item.clazz}]")
      val is = new FileInputStream(item.file)
      zos.putNextEntry(new ZipEntry(item.clazz))
      pipe(is, zos)
      is.close()
      zos.closeEntry()
    })
    zos.close()

    // 3. clear if rm option
    if (rm) {
      files.foreach(f => FileUtils.forceDelete(new File(f.file)))
    }

    tmpFile.delete()
  }

  def update(): Unit = {
    println(s"Current Dir: [${curDir}]")
    packItems.groupBy(_.jar).foreach { case (jar, arr) =>
      updateJar(jar, arr)
    }
  }

  def pack(): Unit = {
    checkBackup()
    update()
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    Args.option("w", false, "Watcher Mode")

    if (args.indexOf("-w") >= 0) {
      watcher.Main.main(args)
      return
    }

    Args.option("lib", true, "To Update Jar Lib Dir")
    Args.option("dir", true, "Class File Dir")
    Args.option("file", true, "File Name Pattern", null)
    Args.option("rm", false, "Need Remove Class File")

    Args.parse(args)

    val libDir = Args.getOptionAsPath("lib")
    val clazzDir = Args.getOptionAsPath("dir")
    val rm = Args.hasOption("rm")

    println(s"Lib: ${libDir}")
    println(s"Dir: ${clazzDir}")
    println(s"Rm: ${rm}")

    //    "D:\\workspace\\scala\\scala-test\\target\\classes\\test"
    val p = new Pack(libDir, clazzDir, rm)
    p.pack()
  }

}
