package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

case class PString(var text: String) extends Userdata {
	override def objID(obj: PObject) = objID(text)
	override def toString = text
}