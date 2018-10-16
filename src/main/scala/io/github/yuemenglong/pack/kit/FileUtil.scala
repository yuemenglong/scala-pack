package io.github.yuemenglong.pack.kit

import java.io.File

object FileUtil {
  val origin: String = System.getProperty("user.dir")

  def restoreWorkDir(): Unit = {
    changeWorkDir(origin)
  }

  def changeWorkDir(s: String): Unit = {
    val dir = new File(s).getAbsoluteFile
    val succ = dir.exists() && dir.isDirectory && System.setProperty("user.dir", dir.getAbsolutePath) != null
    if (!succ) {
      throw new RuntimeException("Change Work Dir Fail")
    }
  }

}
