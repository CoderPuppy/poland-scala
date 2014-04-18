package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

class PRawFunction(ground: PObject, seq: InstructionSeq) extends Userdata {
	override def isCallable = true
	override def call(send: Send) = seq.eval(ground.runtime.createCallGround(ground, this, send))
}