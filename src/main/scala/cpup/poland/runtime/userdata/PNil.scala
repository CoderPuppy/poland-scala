package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

trait TNil extends Userdata {
	override def objID(obj: PObject) = getClass.getName
	override def toString = "nil"

	override def isBoolean = true
	override def toBoolean = false
}
object PNil extends TNil