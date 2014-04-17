package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

class PRuntime extends TRuntime {
	val root = new PObject(this)
	val nil = PNil.createObject(this)

	root(PSymbol("nil")) = nil

	final val modifySymbol = "internal:modifySymbol"
	final val modifyString = "internal:modifyString"
	final val modifyMessage = "internal:modifyMessage"
	final val modifyMessageSeq = "internal:modifyMessageSeq"
	final val modifySend = "internal:modifySend"

	def initSymbols(ground: PObject) {
		ground.symbols.put(modifySymbol, PSymbol(modifySymbol).createObject(this))
		ground.symbols.put("nil", PSymbol("nil").createObject(this))
		ground.symbols.put(modifySymbol, createSymbol(root, PSymbol(modifySymbol)))
		ground.symbols.put("nil", createSymbol(root, PSymbol("nil")))
	}

	override def createSymbol(ground: PObject, sym: PSymbol) = {
		if(!ground.symbols.contains(modifySymbol)) {
			initSymbols(ground)
		}

		val obj = sym.createObject(this)
		ground.send(this, ground.symbols(modifySymbol), obj)
		obj
	}

	override def createString(ground: PObject, str: PString) = {
		val obj = str.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifyString)), obj)
		obj
	}

	override def createMessage(ground: PObject, msg: Message) = {
		val obj = msg.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifyMessage)), obj)
		obj
	}

	override def createMessageSeq(ground: PObject, seq: MessageSeq) = {
		val obj = seq.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifyMessageSeq)), obj)
		obj
	}

	override def createSend(ground: PObject, send: Send) = {
		val obj = send.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifySend)), obj)
		obj
	}
}