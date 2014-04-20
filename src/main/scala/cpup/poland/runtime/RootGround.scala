package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.NativeInstruction
import cpup.poland.runtime.userdata.PSymbol


object RootGround {
	def create = {
		val runtime = new PRuntime
		val polandGround = runtime.root
		val nil = runtime.nil
		val ground = polandGround.derive

		polandGround(PSymbol(PRuntime.Names.modifySymbol)) = runtime.createRawFunction(
			polandGround, new PRawFunction(
				ground,
				InstructionSeq(
					new NativeInstruction((context: SendContext) => {
						println(context.ground.hints("send").userdata match {
							case send: Send => send.msg.args(0).activate(context)
							case _ => null
						})
						runtime.nil
					})
				)
			)
		)

		runtime.initSymbols(polandGround)

		(runtime, ground)
	}
}