package cpup.poland.runtime.userdata

import cpup.poland.runtime.{BaseRuntime, PObject}

case class NativeInstruction(fn: (SendContext) => PObject) extends Userdata with TInstruction {
	override def activate(context: SendContext, seq: InstructionSeq) = {
		fn(context)
	}
}