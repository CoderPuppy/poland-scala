package cpup.poland.runtime

import cpup.poland.runtime.userdata.PSymbol

object RootGround {
	def create = {
		val runtime = new PRuntime
		val polandGround = runtime.root
		val nil = runtime.nil
		val ground = polandGround.derive

		runtime.initSymbols(polandGround)

		(runtime, ground)
	}
}