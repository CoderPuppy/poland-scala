package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

case class Send(ground: PObject, receiver: PObject, seq: MessageSeq, msg: Message) extends Userdata {
	def send = new PObject

	def id = (ground.id, receiver.id, seq.id, msg.id).toString
}

object Send {
	def apply(receiver: PObject, name: PObject, args: PObject*): Send = {
		val ground = new PObject

		for((arg, i) <- args.view.zipWithIndex) {
			ground(PSymbol(s"${getClass.getName}:sendobjs:$i")) = arg
		}

		val msg = new Message(name, args.view.zipWithIndex.map((e) => {
			MessageSeq(Message(PSymbol(s"${getClass.getName}:sendobjs:${e._2}").createObject))
		}).toList)

		Send(ground, receiver, MessageSeq(msg), msg)
	}
}