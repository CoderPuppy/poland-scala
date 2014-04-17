package cpup.poland.runtime.userdata

case class JavaObject(obj: Any) extends Userdata {
	def id = s"${obj.getClass.getName}:${obj.hashCode}"
}