package io.github.yuemenglong.pack.jvm.common

/**
  * Created by <yuemenglong@126.com> on 2018/2/11.
  */

trait AccessFlagName {
  val access_flags: Short

  def accessMaskMap: Map[Int, String]

  private def matche(mask: Int): Boolean = (access_flags & mask) > 0

  def accessFlags: Array[String] = {
    accessMaskMap.toArray.map { case (mask, ret) =>
      matche(mask) match {
        case true => ret
        case false => null
      }
    }.filter(_ != null)
  }
}
