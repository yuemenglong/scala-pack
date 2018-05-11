package io.github.yuemenglong.pack

import java.io._
import java.nio.file.Paths
import java.util.jar.JarFile

import io.github.yuemenglong.pack.jvm.common.StreamReader
import io.github.yuemenglong.pack.jvm.struct.ClassFile
import org.apache.commons.cli._

case class PackItem(file: String, clazz: String, pkg: String, jar: String)

class Pack(libDir: String, clazzDir: String) {
  // class -> jar
  val libMap: Map[String, String] = scanLibJar(libDir)
  // file, package, jar
  val packItems: Array[PackItem] = scanLocalClass(clazzDir)

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
      val entries = jarFile.entries()
      Stream.continually({
        entries.hasMoreElements match {
          case true => entries.nextElement()
          case false => null
        }
      }).takeWhile(_ != null).filter(_.getName.endsWith(".class"))
        .map(f => (f.getName, file.getAbsolutePath))
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

    dir.listFiles().filter(_.getName.endsWith(".class"))
      .map(f => {
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

  def checkBackup(): Unit = {
    packItems.map(_.jar).toSet[String].foreach(path => {
      val backPath = s"${path}.bak"
      if (!new File(backPath).exists()) {
        println(s"[${path}] BackUp")
        exec(s"cp ${path} ${backPath}")
      } else {
        println(s"[${backPath}] Exists")
      }
    })
  }

  def update(): Unit = {
    packItems.foreach(item => {
      val pkgPath = Paths.get(clazzDir, item.pkg)
      val rootPkgPath = Paths.get(clazzDir, item.pkg.split("/")(0))
      exec(s"mkdir -p ${pkgPath}")
      exec(s"mv ${item.file} ${pkgPath}")
      exec(s"jar -uvf ${item.jar} -C ${clazzDir} ${item.clazz}")
      exec(s"rm -rf ${rootPkgPath}")
    })
  }

  def pack(): Unit = {
    checkBackup()
    update()
  }

  def exec(cmd: String): Unit = {
    println(cmd)
    val ex = Runtime.getRuntime.exec(cmd); //添加要进行的命令，"cmd  /c
    {
      val br = new BufferedReader(new InputStreamReader(ex.getInputStream)) //虽然cmd命令可以直接输出，但是通过IO流技术可以保证对数据进行一个缓冲。
      Stream.continually(br.readLine()).takeWhile(_ != null).foreach(println)
    }
    {
      val br = new BufferedReader(new InputStreamReader(ex.getErrorStream))
      val err = Stream.continually(br.readLine()).takeWhile(_ != null).mkString("\n")
      if (err.length > 0) {
        throw new Exception(err)
      }
    }

  }
}

object Main {

  def main(args: Array[String]): Unit = {

    val options = new Options

    val input = new Option("lib", true, "To Update Jar Lib Dir")
    input.setRequired(true)
    options.addOption(input)

    val output = new Option("dir", true, "Class File Dir")
    output.setRequired(true)
    options.addOption(output)

    val parser = new DefaultParser
    val formatter = new HelpFormatter
    val cmd: CommandLine = try {
      parser.parse(options, args)
    } catch {
      case _: Throwable =>
        formatter.printHelp("scala-pack", options)
        System.exit(1)
        null
    }

    val libDir = cmd.getOptionValue("lib") match {
      case s => new File(s).getAbsolutePath
    }
    val clazzDir = cmd.getOptionValue("dir") match {
      case s => new File(s).getAbsolutePath
    }
    println(s"Lib: ${libDir}")
    println(s"Dir: ${clazzDir}")
    //    "D:\\workspace\\scala\\scala-test\\target\\classes\\test"
    val p = new Pack(libDir, clazzDir)
    p.pack()
  }
}
