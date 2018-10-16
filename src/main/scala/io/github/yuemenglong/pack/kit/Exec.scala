package io.github.yuemenglong.pack.kit

import java.io.{BufferedReader, File, InputStreamReader}

class Exec(dir: String = System.getProperty("user.dir")) {
  def exec(args: String*): Array[String] = {
    val (out, err) = exec2(args: _*)
    if (err.length > 0) {
      throw new Exception(err.mkString("\n"))
    }
    out
  }

  def exec2(args: String*): (Array[String], Array[String]) = {
    println(args.mkString(" "))
    val pb = new ProcessBuilder(args: _*)
    pb.directory(new File(dir))
    pb.start()
    val ex = pb.start(); //添加要进行的命令，"cmd  /c
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
}
