package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PObject, PRuntime}

class InstructionSeq(_instrs: TInstruction*) extends Userdata {
	def foreach[R](f: (TInstruction) => R) {
		instrs.foreach(f)
	}

	var instrs = _instrs.toBuffer

	def add(instr: TInstruction) {
		instrs += instr
	}

	override def toString = s"{ ${instrs.mkString(", ")} }"

	def activate(context: SendContext, seq: InstructionSeq) = {
		var current = context.receiver

		for(msg <- this) {
			current = msg.activate(context.withReceiver(current), seq)
		}

		current
	}

	def activate(context: SendContext): PObject = activate(context, this)
}
object InstructionSeq {
	def apply(instrs: TInstruction*) = new InstructionSeq(instrs: _*)
	def unapply(seq: InstructionSeq) = Some(seq.instrs.toList)
}