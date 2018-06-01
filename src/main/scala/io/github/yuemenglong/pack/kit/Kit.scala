package io.github.yuemenglong.pack.kit

import java.io._
import java.text.SimpleDateFormat
import java.util.Date

import io.github.yuemenglong.pack.PackItem
import java.net.URL
import java.net.URLDecoder

object Kit {
  def execs(cmd: String): Stream[String] = {
    println(cmd)
    val ex = Runtime.getRuntime.exec(cmd); //添加要进行的命令，"cmd  /c
    val br = new BufferedReader(new InputStreamReader(ex.getInputStream)) //虽然cmd命令可以直接输出，但是通过IO流技术可以保证对数据进行一个缓冲。
    Stream.continually(br.readLine()).takeWhile(_ != null)
  }

  def exec(cmd: String): Array[String] = {
    val res = exec2(cmd)
    if (res._2.length > 0) {
      throw new Exception(res._2.mkString("\n"))
    }
    res._1
  }

  def exec2(cmd: String): (Array[String], Array[String]) = {
    println(cmd)
    val ex = Runtime.getRuntime.exec(cmd); //添加要进行的命令，"cmd  /c
    val out = {
      val br = new BufferedReader(new InputStreamReader(ex.getInputStream)) //虽然cmd命令可以直接输出，但是通过IO流技术可以保证对数据进行一个缓冲。
      Stream.continually(br.readLine()).takeWhile(_ != null).toArray
    }
    val err = {
      val br = new BufferedReader(new InputStreamReader(ex.getErrorStream))
      Stream.continually(br.readLine()).takeWhile(_ != null).toArray
    }
    if (out.length > 0) {
      println(out.slice(0, 10).mkString("\n"))
    }
    if (err.length > 0) {
      println(err.slice(0, 10).mkString("\n"))
    }
    (out, err)
  }

  def datetime(g1: String = "-", g2: String = " ", g3: String = ":"): String = {
    val f = new SimpleDateFormat(s"yyyy${g1}MM${g1}dd${g2}HH${g3}mm${g3}ss")
    f.format(new Date)
  }

  def datetime2(): String = datetime("_", "_", "_")

  def writer(path: String): BufferedWriter = {
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path)))
  }

  def reader(is: InputStream): BufferedReader = {
    new BufferedReader(new InputStreamReader(is))
  }

  def pipe(is: InputStream, os: OutputStream): Unit = {
    val buffer = new Array[Byte](4 * 1024)
    Stream.continually(is.read(buffer)).takeWhile(_ >= 0).foreach(read => {
      os.write(buffer, 0, read)
    })
  }

  def getThisJarFile: File = {
    val url = classOf[PackItem].getProtectionDomain.getCodeSource.getLocation
    val filePath = URLDecoder.decode(url.getPath, "utf-8") match {
      case s if s.endsWith(".jar") => s.substring(0, s.lastIndexOf("/") + 1)
      case s => s
    }
    new File(filePath)
  }
}
