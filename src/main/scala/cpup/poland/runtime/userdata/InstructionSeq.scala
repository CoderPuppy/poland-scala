package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PObject, PRuntime, Interpreter}

class InstructionSeq(_instrs: TInstruction*) extends Userdata {
	def foreach[R](f: (TInstruction) => R) {
		instrs.foreach(f)
	}

	var instrs = _instrs.toBuffer

	def add(instr: TInstruction) {
		instrs += instr
	}

	override def toString = s"{ ${instrs.mkString(", ")} }"

	def eval(runtime: PRuntime, ground: PObject, current: PObject) = Interpreter.eval(runtime, ground, this, current)
	def eval(runtime: PRuntime, ground: PObject) = Interpreter.eval(runtime, ground, this)
	def eval(runtime: PRuntime) = Interpreter.eval(runtime, this)
	def eval(ground: PObject) = Interpreter.eval(ground, this)
}
object InstructionSeq {
	def apply(instrs: TInstruction*) = new InstructionSeq(instrs: _*)
	def unapply(seq: InstructionSeq) = Some(seq.instrs.toList)
}