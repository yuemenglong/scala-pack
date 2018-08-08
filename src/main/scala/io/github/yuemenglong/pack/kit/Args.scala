package io.github.yuemenglong.pack.kit

import java.io.File

import org.apache.commons.cli._

object Args {
  private val options = new Options
  private var dftMap: Map[String, String] = Map()
  private var cmd: CommandLine = _

  private val DEFAULT_FLAG = "$$$DEFAULT_FLAG$$$"

  def option(opt: String, hasArg: Boolean, desc: String, dft: String = DEFAULT_FLAG): Unit = {
    val option = new Option(opt, hasArg, desc)
    (hasArg, dft) match {
      case (false, _) => //没有参数
      case (true, DEFAULT_FLAG) => option.setRequired(true) // 需要参数且没有传递默认
      case (true, _) => dftMap += (opt -> dft)
    }
    options.addOption(option)
  }

  def printUsage(tips: String = null): Unit = {
    val cls = tips match {
      case null => " "
      case s if s.trim == "" => " "
      case s => s"[${s}]"
    }
    val formatter = new HelpFormatter
    formatter.printHelp(cls, options)
    System.exit(-1)
  }

  def parse(args: Array[String], tips: String = null): Unit = {
    val parser = new DefaultParser
    try {
      cmd = parser.parse(options, args, true)
    } catch {
      case _: Throwable => printUsage()
    }
  }

  def getOptionValue(opt: String): String = {
    if (cmd == null) {
      throw new Exception("Parse Args First")
    }
    cmd.hasOption(opt) match {
      case true => cmd.getOptionValue(opt)
      case false => dftMap(opt)
    }
  }

  def getOptionAsPath(opt: String): String = {
    getOptionValue(opt) match {
      case null => null
      case s => new File(s).getAbsolutePath
    }
  }

  def getOptionAsPaths(opt: String): Array[String] = {
    getOptionValue(opt) match {
      case null => null
      case s => s.split(";").map(s => new File(s).getAbsolutePath)
    }
  }

  def hasOption(opt: String): Boolean = {
    require(cmd != null)
    cmd.hasOption(opt)
  }
}
