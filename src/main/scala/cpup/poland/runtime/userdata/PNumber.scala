package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

case class PNumber(num: Double) extends Userdata {
	override def toString = num.toString
	override def objID(obj: PObject): String = objID(num.toString)
}