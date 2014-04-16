package cpup.poland.runtime.userdata

case class PString(var text: String) extends Userdata {
	def id = text
	override def toString = s"'${text}'"
}