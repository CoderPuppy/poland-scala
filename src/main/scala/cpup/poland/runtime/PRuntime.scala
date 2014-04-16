package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

case class PRuntime(root: PObject) extends TRuntime with TBaseRuntime {
	final val nilUserdata = new PNil
	var _nil: PObject = null
	override def nil = {
		if(_nil == null) {
			_nil = root(this, getSymbol("nil"))
			_nil.userdata = nilUserdata
		}
		_nil
	}

	symbols.put("modifySymbol", PSymbol("modifySymbol").createObject)
	symbols.put("nil", PSymbol("nil").createObject)
	symbols.put("modifySymbol", createSymbol(PSymbol("modifySymbol")))
	symbols.put("nil", createSymbol(PSymbol("nil")))

	override def createSymbol(sym: PSymbol) = {
		val obj = super.createSymbol(sym)
		root.send(this, symbols("modifySymbol"), obj)
		obj
	}

	override def createString(str: PString) = {
		val obj = super.createString(str)
		root.send(this, getSymbol(PSymbol("modifyString")), obj)
		obj
	}

	override def createMessage(msg: Message) = {
		val obj = super.createMessage(msg)
		root.send(this, getSymbol(PSymbol("modifyMessage")), obj)
		obj
	}

	override def createMessageSeq(seq: MessageSeq) = {
		val obj = super.createMessageSeq(seq)
		root.send(this, getSymbol(PSymbol("modifyMessageSeq")), obj)
		obj
	}

	override def createSend(send: Send) = {
		val obj = super.createSend(send)
		root.send(this, getSymbol(PSymbol("modifySend")), obj)
		obj
	}
}