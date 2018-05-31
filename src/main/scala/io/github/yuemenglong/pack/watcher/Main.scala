package io.github.yuemenglong.pack.watcher

import java.io.File
import java.nio.file.Paths

import io.github.yuemenglong.pack.kit.Args
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer

object Main {
  def scan(dir: File, fn: File => Any): Unit = {
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

  def scan(path: String, fn: File => Any): Unit = scan(new File(path), fn)

  def main(args: Array[String]): Unit = {
    Args.option("dir", true, "Watch Dir")
    Args.option("to", true, "Copy Dir")
    Args.option("t", true, "Loop Timeout", "6000")

    Args.parse(args)

    val dir = Args.getOptionAsPath("dir")
    val to = Args.getOptionAsPath("to")
    val timeout = Args.getOptionValue("t").toInt
    println(dir, to)
    var cache = {
      val ab = new ArrayBuffer[(String, Long)]
      scan(dir, (f: File) => {
        ab += ((f.getAbsolutePath, f.lastModified()))
      })
      ab.toMap
    }

    def cacheAndCopy(f: File): Unit = {
      cache += (f.getAbsolutePath -> f.lastModified())
      val dest = Paths.get(to, f.getName).toFile
      FileUtils.copyFile(f, dest)
      println(s"Update ${f.getAbsolutePath}")
    }

    println("Start Watch Loop")
    while (true) {
      Thread.sleep(timeout)
      val start = System.currentTimeMillis()
      scan(dir, (f: File) => {
        cache.get(f.getAbsolutePath) match {
          case None => cacheAndCopy(f)
          case Some(t) => if (f.lastModified() > t) cacheAndCopy(f)
        }
      })
      val end = System.currentTimeMillis()
      println(s"Loop Cost ${end - start}, Files ${cache.size}")
    }
  }
}
