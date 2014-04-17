package cpup.poland.runtime.userdata

import cpup.poland.runtime.{Interpreter, PRuntime, PObject}

class MessageSeq(_msgs: Message*) extends Userdata {
	def id = msgs.map(_.id).mkString(",")

	def foreach[R](f: (Message) => R) {
		msgs.foreach(f)
	}

	var msgs = _msgs.toBuffer

	def add(msg: Message) {
		msgs += msg
	}

	override def toString = s"{ ${msgs.mkString(", ")} }"

	def eval(runtime: PRuntime, ground: PObject, current: PObject) = Interpreter.eval(runtime, ground, this, current)
	def eval(runtime: PRuntime, ground: PObject) = Interpreter.eval(runtime, ground, this)
	def eval(runtime: PRuntime) = Interpreter.eval(runtime, this)
}
object MessageSeq {
	def apply(msgs: Message*) = new MessageSeq(msgs: _*)
	def unapply(seq: MessageSeq) = Some(seq.msgs.toList)
}