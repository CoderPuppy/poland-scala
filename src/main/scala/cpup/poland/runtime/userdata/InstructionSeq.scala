package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PObject, PRuntime, Interpreter}

class InstructionSeq(_msgs: TInstruction*) extends Userdata {
	def foreach[R](f: (TInstruction) => R) {
		msgs.foreach(f)
	}

	var msgs = _msgs.toBuffer

	def add(msg: TInstruction) {
		msgs += msg
	}

	override def toString = s"{ ${msgs.mkString(", ")} }"

	def eval(runtime: PRuntime, ground: PObject, current: PObject) = Interpreter.eval(runtime, ground, this, current)
	def eval(runtime: PRuntime, ground: PObject) = Interpreter.eval(runtime, ground, this)
	def eval(runtime: PRuntime) = Interpreter.eval(runtime, this)
}
object InstructionSeq {
	def apply(msgs: Message*) = new InstructionSeq(msgs: _*)
	def unapply(seq: InstructionSeq) = Some(seq.msgs.toList)
}