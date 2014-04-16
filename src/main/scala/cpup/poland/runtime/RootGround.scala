package cpup.poland.runtime

import cpup.poland.runtime.userdata.PSymbol

object RootGround {
	def create = {
		val root = new PObject
		val ground = new PObject

		val nil = new PObject
		root(PSymbol("nil")) = nil

		(root, ground)
	}
}