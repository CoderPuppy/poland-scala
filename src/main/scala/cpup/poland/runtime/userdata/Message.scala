package cpup.poland.runtime.userdata

import cpup.poland.runtime.{BaseRuntime, PObject}
import cpup.poland.parser.Lexer

class Message(var name: PObject, var pos: Lexer.TokenPos, _args: InstructionSeq*) extends Userdata with TInstruction {
	var args = _args.toBuffer

	override def toString = s"${name.toString}(${args.mkString(", ")})"

	override def activate(context: SendContext, seq: InstructionSeq) = {
		Send(context, seq, this).send
	}
}
object Message {
	def apply(name: PObject, pos: Lexer.TokenPos, args: InstructionSeq*) = new Message(name, pos, args: _*)
	def unapply(msg: Message) = Some((msg.name, msg.args.toList))
}