package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PRuntime, PObject}

case class NativeInstruction(fn: (PRuntime, PObject, PObject) => PObject) extends Userdata with TInstruction {
	override def activate(runtime: PRuntime, ground: PObject, receiver: PObject, seq: InstructionSeq) = {
		fn(runtime, ground, receiver)
	}
}