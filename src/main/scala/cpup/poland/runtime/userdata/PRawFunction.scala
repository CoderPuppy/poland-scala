package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

class PRawFunction(ground: PObject, seq: InstructionSeq) extends Userdata with TInstruction {
	override def isCallable = true
	override def call(send: Send) = {
		val ground = send.context.ground.runtime.createCallGround(send.context.ground, this, send)
		seq.activate(SendContext(ground, ground))
	}

	override def activate(context: SendContext, seq: InstructionSeq) = seq.activate(context) // TODO: maybe?
}