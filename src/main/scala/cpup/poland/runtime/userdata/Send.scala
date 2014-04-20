package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PRuntime, PObject}
import cpup.poland.parser.Lexer

case class Send(context: SendContext, seq: InstructionSeq, msg: Message) extends Userdata {
	def send: PObject = context.receiver.receive(this)
}

object Send {
	final val name = getClass.getName

	def fromObjs(ground: PObject, receiver: PObject, name: PObject, args: PObject*): Send = {
		val msg = new Message(name, Lexer.TokenPos(s"${this.name}:sendobjs", 1, 1, 1), args.map((obj) => {
			InstructionSeq(InjectObjectInstruction(obj))
		}).toSeq: _*)

		Send(SendContext(ground, receiver), InstructionSeq(msg), msg)
	}
}