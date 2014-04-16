package cpup.poland.runtime

import cpup.poland.runtime.userdata._

trait TRuntime {
	def createSymbol(sym: PSymbol): PObject
	def createString(str: PString): PObject
	def createMessage(msg: Message): PObject
	def createMessageSeq(seq: MessageSeq): PObject
	def createSend(send: Send): PObject
	def createNil(nil: PNil): PObject
}