package cpup.poland.runtime.userdata

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
}
object MessageSeq {
	def apply(msgs: Message*) = new MessageSeq(msgs: _*)
	def unapply(seq: MessageSeq) = Some(seq.msgs.toList)
}