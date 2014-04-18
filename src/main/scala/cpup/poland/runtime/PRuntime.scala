package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

class PRuntime {
	val root = new PObject(this)
	val nil = PNil.createObject(this)

	root(PSymbol("nil")) = nil

	final val modifySymbol = "internal:modifySymbol"
	final val modifyString = "internal:modifyString"
	final val modifyMessage = "internal:modifyMessage"
	final val modifyMessageSeq = "internal:modifyMessageSeq"
	final val modifySend = "internal:modifySend"
	final val modifyRawFunction = "internal:modifyRawFunction"
	final val modifyCallGround = "internal:modifyCallGround"

	def initSymbols(ground: PObject) {
		ground.hints.put(s"symbol:$modifySymbol", PSymbol(modifySymbol).createObject(this))
		ground.hints.put("symbol:nil", PSymbol("nil").createObject(this))
		ground.hints.put(s"symbol:$modifySymbol", createSymbol(root, PSymbol(modifySymbol)))
		ground.hints.put("symbol:nil", createSymbol(root, PSymbol("nil")))
	}

	def createSymbol(ground: PObject, sym: PSymbol) = {
		if(!ground.hints.contains(s"symbol:$modifySymbol")) {
			initSymbols(ground)
		}

		val obj = sym.createObject(this)
		ground.send(this, ground.hints(s"symbol:$modifySymbol"), obj)
		obj
	}

	def getSymbol(ground: PObject, sym: PSymbol) = ground.hints.getOrElseUpdate(s"symbol:${sym.text}", createSymbol(ground, sym))
	def getSymbol(ground: PObject, sym: String): PObject = getSymbol(ground, PSymbol(sym))

	def createString(ground: PObject, str: PString) = {
		val obj = str.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifyString)), obj)
		obj
	}

	def createMessage(ground: PObject, msg: Message) = {
		val obj = msg.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifyMessage)), obj)
		obj
	}

	def createMessageSeq(ground: PObject, seq: InstructionSeq) = {
		val obj = seq.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifyMessageSeq)), obj)
		obj
	}

	def createSend(ground: PObject, send: Send) = {
		val obj = send.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifySend)), obj)
		obj
	}

	def createRawFunction(ground: PObject, fn: PRawFunction) = {
		val obj = fn.createObject(this)
		ground.send(this, getSymbol(ground, PSymbol(modifyRawFunction)), obj)
		obj
	}

	def createCallGround(ground: PObject, fn: PRawFunction, send: Send) = {
		val obj = createObject
		obj.hints("fn") = fn.createObject(this)
		obj.hints("send") = send.createObject(this)
		ground.send(this, modifyCallGround, obj, createRawFunction(ground, fn), createSend(ground, send))
		obj
	}

	def createObject = new PObject(this)
}