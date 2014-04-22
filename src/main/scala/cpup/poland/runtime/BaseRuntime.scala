package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

class BaseRuntime {
	val root = new PObject(this)
	val nil = PNil.createObject(this)
	val symbols = new PObject(this)

	root(PSymbol("nil")) = nil
	root(PSymbol(BaseRuntime.Names.symbols)) = symbols

	val initSymbols = List(
		BaseRuntime.Names.modifySymbol,
		BaseRuntime.Names.modifyObject,
		BaseRuntime.Names.modifyUserdata,
		BaseRuntime.Names.modifyCallGround,
		BaseRuntime.Names.nil
	) ++ (0 to 10).map((i) => s"${Send.name}:sendobjs:$i")

	def initSymbols(ground: PObject) {
		val symbols = ground(PSymbol(BaseRuntime.Names.symbols))

		for(sym <- initSymbols) {
			symbols(PSymbol(sym)) = PSymbol(sym).createObject(this)
		}

		for(sym <- initSymbols) {
			createSymbol(ground, PSymbol(sym))
		}

		ground.send(BaseRuntime.Names.modifyObject, nil)
		ground.send(BaseRuntime.Names.modifyUserdata, nil)
	}

	def createObject(ground: PObject) = {
		val modifyObject = ground(PSymbol(BaseRuntime.Names.symbols))(PSymbol(BaseRuntime.Names.modifyObject))
		if(modifyObject == ground(PSymbol(BaseRuntime.Names.nil))) {
			initSymbols(ground)
		}

		val obj = new PObject(this)
		ground.send(this, modifyObject, List(obj))
		obj
	}

	def createUserdata(ground: PObject, userdata: Userdata) = {
		val modifyUserdata = ground(PSymbol(BaseRuntime.Names.symbols))(PSymbol(BaseRuntime.Names.modifyUserdata))
		if(modifyUserdata == ground(PSymbol(BaseRuntime.Names.nil))) {
			initSymbols(ground)
		}

		val obj = createObject(ground)
		obj.userdata = userdata
		ground.send(this, modifyUserdata, List(obj))
		obj
	}

	private def createSymbol(ground: PObject, sym: PSymbol) = {
		val modifySymbol = ground(PSymbol(BaseRuntime.Names.symbols))(PSymbol(BaseRuntime.Names.modifySymbol))
		if(modifySymbol == ground(PSymbol(BaseRuntime.Names.nil))) {
			initSymbols(ground)
		}

		val obj = createUserdata(ground, sym)
		ground(PSymbol(BaseRuntime.Names.symbols))(sym) = obj
		ground.send(this, modifySymbol, List(obj))
		obj
	}

	def getSymbol(ground: PObject, sym: PSymbol) = {
		val symbols = ground(PSymbol(BaseRuntime.Names.symbols))
		val obj = symbols(sym)

		if(obj == ground(PSymbol(BaseRuntime.Names.nil))) {
			symbols(sym) = createSymbol(ground, sym)
		} else { obj }
	}
	def getSymbol(ground: PObject, sym: String): PObject = getSymbol(ground, PSymbol(sym))

	def createString(ground: PObject, str: PString) = {
		val obj = createUserdata(ground, str)
		ground.send(this, getSymbol(ground, PSymbol(BaseRuntime.Names.modifyString)), List(obj))
		obj
	}

	def createMessage(ground: PObject, msg: Message) = {
		val obj = createUserdata(ground, msg)
		ground.send(this, getSymbol(ground, PSymbol(BaseRuntime.Names.modifyMessage)), List(obj))
		obj
	}

	def createInstructionSeq(ground: PObject, seq: InstructionSeq) = {
		val obj = createUserdata(ground, seq)
		ground.send(this, getSymbol(ground, PSymbol(BaseRuntime.Names.modifyInstructionSeq)), List(obj))
		obj
	}

	def createSend(ground: PObject, send: Send) = {
		val obj = createUserdata(ground, send)
		ground.send(this, getSymbol(ground, PSymbol(BaseRuntime.Names.modifySend)), List(obj))
		obj
	}

	def createRawFunction(ground: PObject, fn: PRawFunction) = {
		val obj = createUserdata(ground, fn)
		ground.send(this, getSymbol(ground, PSymbol(BaseRuntime.Names.modifyRawFunction)), List(obj))
		obj
	}

	def createCallGround(ground: PObject, fn: PRawFunction, send: Send) = {
		val obj = createObject(ground)
		obj.hints("fn") = fn.createObject(this)
		obj.hints("send") = send.createObject(this)
		ground.send(
			this,
			BaseRuntime.Names.modifyCallGround,
			List(
				obj,
				createRawFunction(ground, fn),
				createSend(ground, send)
			)
		)
		obj
	}
}

object BaseRuntime {
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