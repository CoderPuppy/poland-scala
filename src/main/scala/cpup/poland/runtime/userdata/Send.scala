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
			ground(PSymbol(s"$name:sendobjs:$i")) = arg
		}

		val msg = new Message(name, Lexer.TokenPos(s"$name:sendobjs", 1, 1, 1), args.view.zipWithIndex.map((e) => {
			InstructionSeq(Message(
				PSymbol(s"$name:sendobjs:${e._2}").createObject(runtime),
				Lexer.TokenPos(s"$name:sendobjs", 1, 1, 1)
			))
		}).toSeq: _*)

		Send(SendContext(runtime, ground, receiver), InstructionSeq(msg), msg)
	}
}