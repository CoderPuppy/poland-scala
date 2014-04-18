package cpup.poland.runtime


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