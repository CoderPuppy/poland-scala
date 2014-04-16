package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

case class PRuntime(root: PObject) extends TRuntime with TBaseRuntime {
	var createSymbolName = PSymbol("createSymbol").createObject
	override def createSymbol(sym: PSymbol) = {
		val obj = Send(root, createSymbolName, super.createSymbol(sym)).send
		obj.userdata = sym

		obj.userdata match {
			case PSymbol(text) if text == "createSymbol" =>
				createSymbolName = obj
				
			case _ =>
		}

		obj
	}

	override def createString(str: PString) = {
		val obj = Send(root, createSymbol(PSymbol("createString")), super.createString(str)).send
		obj.userdata = str
		obj
	}

	override def createMessage(msg: Message) = {
		val obj = Send(root, createSymbol(PSymbol("createMessage")), super.createMessage(msg)).send
		obj.userdata = msg
		obj
	}

	override def createMessageSeq(seq: MessageSeq) = {
		val obj = Send(root, createSymbol(PSymbol("createMessageSeq")), super.createMessageSeq(seq)).send
		obj.userdata = seq
		obj
	}

	override def createSend(send: Send) = {
		val obj = Send(root, createSymbol(PSymbol("createSend")), super.createSend(send)).send
		obj.userdata = send
		obj
	}
}