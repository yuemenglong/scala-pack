package io.github.yuemenglong.pack.kit

import org.apache.commons.cli._

object Args {
  private val options = new Options
  private var dftMap: Map[String, String] = Map()
  private var cmd: CommandLine = _

  def option(opt: String, hasArg: Boolean, desc: String, dft: String = null): Unit = {
    val option = new Option(opt, hasArg, desc)
    (hasArg, dft) match {
      case (false, _) => //没有参数
      case (true, null) => option.setRequired(true)
      case (true, _) => dftMap += (opt -> dft)
    }
    options.addOption(option)
  }

  def printUsage(): Unit = {
    val formatter = new HelpFormatter
    formatter.printHelp(" ", options)
  }

  def parse(args: Array[String]): Unit = {
    val parser = new DefaultParser
    val formatter = new HelpFormatter
    try {
      cmd = parser.parse(options, args)
    } catch {
      case _: Throwable =>
        formatter.printHelp(" ", options)
        System.exit(1)
    }
  }

  def getOptionValue(opt: String): String = {
    require(cmd != null)
    cmd.hasOption(opt) match {
      case true => cmd.getOptionValue(opt)
      case false => dftMap(opt)
    }
  }

  def hasOption(opt: String): Boolean = {
    require(cmd != null)
    cmd.hasOption(opt)
  }
}
