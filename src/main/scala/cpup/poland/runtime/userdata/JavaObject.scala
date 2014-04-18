package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

case class JavaObject(obj: Any) extends Userdata {
	def id = s"${obj.getClass.getName}:${obj.hashCode}"
	override def toString = obj.toString
}
trait TNull extends TNil {
	override def objID(obj: PObject) = objID("null")
	override def toString = "null"
}
object PNull extends TNull