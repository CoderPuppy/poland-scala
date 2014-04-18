package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

case class JavaObject(jobj: Any) extends Userdata {
	override def objID(obj: PObject) = (jobj.getClass.getName, jobj.hashCode).toString
	override def toString = jobj.toString
}
trait TNull extends TNil {
	override def toString = "null"
}
object PNull extends TNull