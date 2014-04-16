package cpup.poland.runtime.userdata

class MessageSeq(_msgs: List[Message] = List[Message]()) extends Userdata {
	def id = msgs.map(_.id).mkString(",")

	var msgs = _msgs.toBuffer

	def add(msg: Message) {
		msgs += msg
	}

	override def toString = s"{ ${msgs.mkString(", ")} }"
}
object MessageSeq {
	def apply(msgs: List[Message] = List[Message]()) = new MessageSeq(msgs)
	def unapply(seq: MessageSeq) = Some(seq.msgs.toList)
}