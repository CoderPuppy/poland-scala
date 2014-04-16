package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

trait TBaseRuntime extends TRuntime {
	override def createSend(send: Send) = send.createObject
	override def createMessageSeq(seq: MessageSeq) = seq.createObject
	override def createMessage(msg: Message) = msg.createObject
	override def createString(str: PString) = str.createObject
	override def createSymbol(sym: PSymbol) = sym.createObject
	override def createNil(nil: PNil) = nil.createObject
}
object BaseRuntime extends TBaseRuntime