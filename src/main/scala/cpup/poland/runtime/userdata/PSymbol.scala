package cpup.poland.runtime.userdata

case class PSymbol(text: String) extends Userdata {
	def id = text
	override def toString = s":'${text}'"
}