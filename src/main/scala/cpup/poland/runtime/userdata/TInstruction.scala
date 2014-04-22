package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PObject, BaseRuntime}

trait TInstruction extends Userdata {
	def activate(context: SendContext, seq: InstructionSeq): PObject
}