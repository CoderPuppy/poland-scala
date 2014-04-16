package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

trait TBaseRuntime extends TRuntime {
	private val _nil = new PNil().createObject
	def nil = _nil

	override def createSend(send: Send) = send.createObject
	override def createMessageSeq(seq: MessageSeq) = seq.createObject
	override def createMessage(msg: Message) = msg.createObject
	override def createString(str: PString) = str.createObject
	override def createSymbol(sym: PSymbol) = sym.createObject
}
object BaseRuntime extends TBaseRuntime