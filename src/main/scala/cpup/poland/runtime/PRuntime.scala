package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

class PRuntime {
	val root = new PObject(this)
	val nil = PNil.createObject(this)
	val symbols = new PObject(this)

	root(PSymbol("nil")) = nil
	root(PSymbol(PRuntime.Names.symbols)) = symbols

	val initSymbols = List(
		PRuntime.Names.modifySymbol,
		PRuntime.Names.modifyObject,
		PRuntime.Names.modifyUserdata,
		PRuntime.Names.modifyCallGround,
		PRuntime.Names.nil
	) ++ (0 to 10).map((i) => s"${Send.name}:sendobjs:$i")

	def initSymbols(ground: PObject) {
		val symbols = ground(PSymbol(PRuntime.Names.symbols))

		for(sym <- initSymbols) {
			symbols(PSymbol(sym)) = PSymbol(sym).createObject(this)
		}

		for(sym <- initSymbols) {
			createSymbol(ground, PSymbol(sym))
		}

		ground.send(PRuntime.Names.modifyObject, nil)
		ground.send(PRuntime.Names.modifyUserdata, nil)
	}

	def createObject(ground: PObject) = {
		val modifyObject = ground(PSymbol(PRuntime.Names.symbols))(PSymbol(PRuntime.Names.modifyObject))
		if(modifyObject == ground(PSymbol(PRuntime.Names.nil))) {
			initSymbols(ground)
		}

		val obj = new PObject(this)
		ground.send(this, modifyObject, obj)
		obj
	}

	def createUserdata(ground: PObject, userdata: Userdata) = {
		val modifyUserdata = ground(PSymbol(PRuntime.Names.symbols))(PSymbol(PRuntime.Names.modifyUserdata))
		if(modifyUserdata == ground(PSymbol(PRuntime.Names.nil))) {
			initSymbols(ground)
		}

		val obj = createObject(ground)
		obj.userdata = userdata
		ground.send(this, modifyUserdata, obj)
		obj
	}

	private def createSymbol(ground: PObject, sym: PSymbol) = {
		val modifySymbol = ground(PSymbol(PRuntime.Names.symbols))(PSymbol(PRuntime.Names.modifySymbol))
		if(modifySymbol == ground(PSymbol(PRuntime.Names.nil))) {
			initSymbols(ground)
		}

		val obj = createUserdata(ground, sym)
		ground(PSymbol(PRuntime.Names.symbols))(sym) = obj
		ground.send(this, modifySymbol, obj)
		obj
	}

	def getSymbol(ground: PObject, sym: PSymbol) = {
		val symbols = ground(PSymbol(PRuntime.Names.symbols))
		val obj = symbols(sym)

		if(obj == ground(PSymbol(PRuntime.Names.nil))) {
			symbols(sym) = createSymbol(ground, sym)
		} else { obj }
	}
	def getSymbol(ground: PObject, sym: String): PObject = getSymbol(ground, PSymbol(sym))

	def createString(ground: PObject, str: PString) = {
		val obj = createUserdata(ground, str)
		ground.send(this, getSymbol(ground, PSymbol(PRuntime.Names.modifyString)), obj)
		obj
	}

	def createMessage(ground: PObject, msg: Message) = {
		val obj = createUserdata(ground, msg)
		ground.send(this, getSymbol(ground, PSymbol(PRuntime.Names.modifyMessage)), obj)
		obj
	}

	def createInstructionSeq(ground: PObject, seq: InstructionSeq) = {
		val obj = createUserdata(ground, seq)
		ground.send(this, getSymbol(ground, PSymbol(PRuntime.Names.modifyInstructionSeq)), obj)
		obj
	}

	def createSend(ground: PObject, send: Send) = {
		val obj = createUserdata(ground, send)
		ground.send(this, getSymbol(ground, PSymbol(PRuntime.Names.modifySend)), obj)
		obj
	}

	def createRawFunction(ground: PObject, fn: PRawFunction) = {
		val obj = createUserdata(ground, fn)
		ground.send(this, getSymbol(ground, PSymbol(PRuntime.Names.modifyRawFunction)), obj)
		obj
	}

	def createCallGround(ground: PObject, fn: PRawFunction, send: Send) = {
		val obj = createObject(ground)
		obj.hints("fn") = fn.createObject(this)
		obj.hints("send") = send.createObject(this)
		ground.send(this, PRuntime.Names.modifyCallGround, obj, createRawFunction(ground, fn), createSend(ground, send))
		obj
	}
}

object PRuntime {
	object Names {
		def symbols = "internal:symbols"
		def modifyObject = "internal:modifyObject"
		def modifyUserdata = "internal:modifyUserdata"
		def modifySymbol = "internal:modifySymbol"
		def modifyString = "internal:modifyString"
		def modifyMessage = "internal:modifyMessage"
		def modifyInstructionSeq = "internal:modifyInstructionSeq"
		def modifySend = "internal:modifySend"
		def modifyRawFunction = "internal:modifyRawFunction"
		def modifyCallGround = "internal:modifyCallGround"
		def nil = "nil"
	}
}