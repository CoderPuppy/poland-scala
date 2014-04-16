package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

case class PRuntime(root: PObject) extends TRuntime with TBaseRuntime {
	var createSymbolName = PSymbol("createSymbol").createObject
	override def createSymbol(sym: PSymbol) = {
		val obj = root.send(createSymbolName, super.createSymbol(sym))
		obj.userdata = sym

		obj.userdata match {
			case PSymbol(text) if text == "createSymbol" =>
				createSymbolName = obj
				
			case _ =>
		}

		obj
	}

	override def createString(str: PString) = {
		val obj = root.send(createSymbol(PSymbol("createString")), super.createString(str))
		obj.userdata = str
		obj
	}

	override def createMessage(msg: Message) = {
		val obj = root.send(createSymbol(PSymbol("createMessage")), super.createMessage(msg))
		obj.userdata = msg
		obj
	}

	override def createMessageSeq(seq: MessageSeq) = {
		val obj = root.send(createSymbol(PSymbol("createMessageSeq")), super.createMessageSeq(seq))
		obj.userdata = seq
		obj
	}

	override def createSend(send: Send) = {
		val obj = root.send(createSymbol(PSymbol("createSend")), super.createSend(send))
		obj.userdata = send
		obj
	}

	override def createNil(nil: PNil) = {
		val obj = root.send(createSymbol(PSymbol("createNil")), super.createNil(nil))
		obj.userdata = nil
		obj
	}
}