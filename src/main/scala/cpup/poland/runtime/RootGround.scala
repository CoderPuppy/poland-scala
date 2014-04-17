package cpup.poland.runtime

import cpup.poland.runtime.userdata.PSymbol

object RootGround {
	def create = {
		val ground = new PObject

		val nil = new PObject
		ground(PSymbol("nil")) = nil

		ground
	}
}