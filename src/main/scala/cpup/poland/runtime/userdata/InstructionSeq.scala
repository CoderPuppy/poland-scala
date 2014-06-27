package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

class InstructionSeq(_instrs: TInstruction*) extends Userdata {
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

	def length = instrs.length
	def size = instrs.size
	def apply(i: Int) = instrs(i)
	def foreach[R](f: (TInstruction) => R) {
		instrs.foreach(f)
	}
}
object InstructionSeq {
	def apply(instrs: TInstruction*) = new InstructionSeq(instrs: _*)
	def unapply(seq: InstructionSeq) = Some(seq.instrs.toList)
}