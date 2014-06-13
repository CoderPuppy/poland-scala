package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

case class PSymbol(text: String) extends Userdata {
	override def objID(obj: PObject) = objID(text)
	override def toString = s":'$text'"
}