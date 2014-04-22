package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

sealed case class PBoolean(v: Boolean) extends Userdata {
	override def objID(obj: PObject) = objID(v.toString)

	override def isBoolean = true
	override def toBoolean = v
}

object PTrue extends PBoolean(true)
object PFalse extends PBoolean(false)