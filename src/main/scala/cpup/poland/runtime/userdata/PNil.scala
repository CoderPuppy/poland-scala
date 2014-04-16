package cpup.poland.runtime.userdata

class PNil extends Userdata {
	final val id = getClass.getName

	override def toString = "nil"

	override def isCallable = true
	override def call(send: Send) = {
		throw new RuntimeException("Attempt to call nil")
	}
}