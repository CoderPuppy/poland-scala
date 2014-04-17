package cpup.poland.runtime.userdata

trait TNil extends Userdata {
	override def id = "nil"
	override def toString = "nil"

	override def isBoolean = true
	override def toBoolean = false
}
object PNil extends TNil