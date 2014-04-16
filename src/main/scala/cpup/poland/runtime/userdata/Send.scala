package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PRuntime, PObject}
import cpup.poland.parser.Lexer

case class Send(ground: PObject, receiver: PObject, seq: MessageSeq, msg: Message) extends Userdata {
	val runtime = PRuntime(ground)

	def send: PObject = receiver.receive(this)

	def id = (ground.id, receiver.id, seq.id, msg.id).toString
}

object Send {
	final val name = getClass.getName

	def apply(root: PObject, receiver: PObject, name: PObject, args: PObject*): Send = {
		val ground = root.derive

		for((arg, i) <- args.view.zipWithIndex) {
			ground(PSymbol(s"$name:sendobjs:$i")) = arg
		}

		val msg = new Message(name, Lexer.TokenPos(s"$name:sendobjs", 1, 1, 1), args.view.zipWithIndex.map((e) => {
			MessageSeq(Message(
				PSymbol(s"$name:sendobjs:${e._2}").createObject,
				Lexer.TokenPos(s"$name:sendobjs", 1, 1, 1)
			))
		}).toSeq: _*)

		Send(ground, receiver, MessageSeq(msg), msg)
	}
}