package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

class Message(var name: PObject, _args: List[MessageSeq] = List[MessageSeq]()) extends Userdata {
	def id = (name.id, args.map(_.id).mkString(",")).toString

	var args = _args.toBuffer

	override def toString = s"${name.toString}(${args.mkString(", ")})"
}
object Message {
	def apply(name: PObject, args: List[MessageSeq] = List[MessageSeq]()) = new Message(name, args)
	def unapply(msg: Message) = Some((msg.name, msg.args.toList))
}