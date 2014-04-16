package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject
import cpup.poland.parser.Lexer

class Message(var name: PObject, pos: Lexer.TokenPos, _args: MessageSeq*) extends Userdata {
	def id = (name.id, args.map(_.id).mkString(",")).toString

	var args = _args.toBuffer

	override def toString = s"${name.toString}(${args.mkString(", ")})"
}
object Message {
	def apply(name: PObject, pos: Lexer.TokenPos, args: MessageSeq*) = new Message(name, pos, args: _*)
	def unapply(msg: Message) = Some((msg.name, msg.args.toList))
}