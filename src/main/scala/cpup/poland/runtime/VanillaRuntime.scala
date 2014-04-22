package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.NativeInstruction
import cpup.poland.runtime.userdata.PSymbol

class VanillaRuntime extends BaseRuntime {
	val polandGround = root
	val ground = polandGround.derive

	polandGround(PSymbol(BaseRuntime.Names.modifySymbol)) = createRawFunction(
		polandGround, new PRawFunction(
			ground,
			InstructionSeq(
				new NativeInstruction((context: SendContext) => {
					println(context.ground.hints("send").userdata match {
						case send: Send => send.msg.args(0).activate(context)
						case _ => null
					})
					nil
				})
			)
		)
	)

	initSymbols(polandGround)
}