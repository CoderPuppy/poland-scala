package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

case class InjectObjectInstruction(obj: PObject) extends Userdata with TInstruction {
	override def activate(context: SendContext, seq: InstructionSeq) = obj
}