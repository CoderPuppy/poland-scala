package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.NativeInstruction
import cpup.poland.runtime.userdata.PSymbol

class VanillaRuntime extends BaseRuntime {
	val PolandGround = root
	val Ground = PolandGround.derive

	val Object = new PObject(this) // TODO: better name for Object and Something
	val VanillaBehaviour = new PObject(this)
	val Something = Object.derive.deriveFrom(VanillaBehaviour)

	PolandGround(PSymbol(PNames.modifySymbol)) = createRawFunction(
		root,
		new PRawFunction(
			Ground,
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

	// Add all the objects to Ground
	Ground(PSymbol("PolandGround")) = PolandGround
	Ground(PSymbol("Object")) = Object
	Ground(PSymbol("VanillaBehavior")) = VanillaBehaviour
	Ground(PSymbol("Something")) = Something
	Ground(PSymbol("Ground")) = Ground

	initSymbols(PolandGround)
}