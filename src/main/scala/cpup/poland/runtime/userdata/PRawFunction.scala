package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

class PRawFunction(ground: PObject, seq: InstructionSeq) extends Userdata with TInstruction {
	override def isActivatable = true
	override def activate(send: Send) = {
		val ground = this.ground.runtime.createCallGround(
			this.ground,
			this,
			send
		)
		seq.activate(SendContext(ground, ground))
	}

	override def activate(context: SendContext, seq: InstructionSeq) = seq.activate(context) // TODO: maybe?
}