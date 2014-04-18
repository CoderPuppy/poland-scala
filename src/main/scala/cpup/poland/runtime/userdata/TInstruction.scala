package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PObject, PRuntime}

trait TInstruction extends Userdata {
	def activate(runtime: PRuntime, ground: PObject, receiver: PObject, seq: InstructionSeq): PObject
}