package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PRuntime, PObject}
import cpup.poland.parser.Lexer

class Message(var name: PObject, pos: Lexer.TokenPos, _args: InstructionSeq*) extends Userdata with TInstruction {
	var args = _args.toBuffer

	override def toString = s"${name.toString}(${args.mkString(", ")})"

	override def activate(runtime: PRuntime, ground: PObject, receiver: PObject, seq: InstructionSeq) = {
		Send(SendContext(runtime, ground, receiver), seq, this).send
	}
}
object Message {
	def apply(name: PObject, pos: Lexer.TokenPos, args: InstructionSeq*) = new Message(name, pos, args: _*)
	def unapply(msg: Message) = Some((msg.name, msg.args.toList))
}