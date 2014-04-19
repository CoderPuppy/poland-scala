package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PRuntime, PObject}
import cpup.poland.parser.Lexer

case class Send(context: SendContext, seq: InstructionSeq, msg: Message) extends Userdata {
	def send: PObject = context.receiver.receive(this)
}

object Send {
	final val name = getClass.getName

	def fromObjs(runtime: PRuntime, root: PObject, receiver: PObject, name: PObject, args: PObject*): Send = {
		val ground = root.derive

		for((arg, i) <- args.view.zipWithIndex) {
			ground(PSymbol(s"${this.name}:sendobjs:$i")) = arg
		}

		val msg = new Message(name, Lexer.TokenPos(s"${this.name}:sendobjs", 1, 1, 1), args.view.zipWithIndex.map((e) => {
			InstructionSeq(Message(
				runtime.getSymbol(root, s"${this.name}:sendobjs:${e._2}"),
				Lexer.TokenPos(s"${this.name}:sendobjs", 1, 1, 1)
			))
		}).toSeq: _*)

		Send(SendContext(runtime, ground, receiver), InstructionSeq(msg), msg)
	}
}