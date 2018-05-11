package io.github.yuemenglong.pack.jvm.nativ

import io.github.yuemenglong.pack.jvm.rt.Vm
import io.github.yuemenglong.pack.jvm.struct.ClassFile
import io.github.yuemenglong.pack.jvm.rt.Vm
import io.github.yuemenglong.pack.jvm.struct.ClassFile

/**
  * Created by <yuemenglong@126.com> on 2018/2/23.
  */
abstract class Cls extends Obj(Vm.rt.load("java/lang/Class")) {}

class ClsT(val tcf: ClassFile) extends Cls {}

class ClsA(val t: String, val dim: Int) extends Cls {}

class ClsAT(val tcf: ClassFile, val dim: Int) extends Cls {}

