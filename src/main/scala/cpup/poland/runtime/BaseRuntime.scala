package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import cpup.poland.runtime.userdata.PString
import cpup.poland.runtime.userdata.PSymbol

class BaseRuntime {
	val root = new PObject(this)
	val nil = PNil.createObject(this)
	val ptrue = PTrue.createObject(this)
	val pfalse = PFalse.createObject(this)
	val symbols = new PObject(this)

	root(PSymbol(PNames.nil)) = nil
	root(PSymbol("true")) = ptrue
	root(PSymbol("false")) = pfalse
	root(PSymbol(PNames.symbols)) = symbols

	val initSymbols = List(
		PNames.modifySymbol,
		PNames.modifyObject,
		PNames.modifyUserdata,
		PNames.modifyCallGround,
		PNames.nil
	) ++ (0 to 10).map((i) => s"${Send.name}:sendobjs:$i")

	def initSymbols(ground: PObject) {
		val symbols = ground(PSymbol(PNames.symbols))

		for(sym <- initSymbols) {
			symbols(PSymbol(sym)) = PSymbol(sym).createObject(this)
		}

		for(sym <- initSymbols) {
			createSymbol(ground, PSymbol(sym))
		}

		ground.send(PNames.modifyObject, nil)
		ground.send(PNames.modifyUserdata, nil)
	}

	def createObject(ground: PObject) = {
		val modifyObject = ground(PSymbol(PNames.symbols))(PSymbol(PNames.modifyObject))
		if(modifyObject == ground(PSymbol(PNames.nil))) {
			initSymbols(ground)
		}

		val obj = new PObject(this)
		ground.send(this, modifyObject, List(obj))
		obj
	}

	def createUserdata(ground: PObject, userdata: Userdata) = {
		val modifyUserdata = ground(PSymbol(PNames.symbols))(PSymbol(PNames.modifyUserdata))
		if(modifyUserdata == ground(PSymbol(PNames.nil))) {
			initSymbols(ground)
		}

		val obj = createObject(ground)
		obj.userdata = userdata
		ground.send(this, modifyUserdata, List(obj))
		obj
	}

	private def createSymbol(ground: PObject, sym: PSymbol) = {
		val modifySymbol = ground(PSymbol(PNames.symbols))(PSymbol(PNames.modifySymbol))
		if(modifySymbol == ground(PSymbol(PNames.nil))) {
			initSymbols(ground)
		}

		val obj = createUserdata(ground, sym)
		ground(PSymbol(PNames.symbols))(sym) = obj
		ground.send(this, modifySymbol, List(obj))
		obj
	}

	def getSymbol(ground: PObject, sym: PSymbol) = {
		val symbols = ground(PSymbol(PNames.symbols))
		val obj = symbols(sym)

		if(obj == ground(PSymbol(PNames.nil))) {
			symbols(sym) = createSymbol(ground, sym)
		} else { obj }
	}
	def getSymbol(ground: PObject, sym: String): PObject = getSymbol(ground, PSymbol(sym))

	def createString(ground: PObject, str: PString) = {
		val obj = createUserdata(ground, str)
		ground.send(this, getSymbol(ground, PSymbol(PNames.modifyString)), List(obj))
		obj
	}

	def createMessage(ground: PObject, msg: Message) = {
		val obj = createUserdata(ground, msg)
		ground.send(this, getSymbol(ground, PSymbol(PNames.modifyMessage)), List(obj))
		obj
	}

	def createInstructionSeq(ground: PObject, seq: InstructionSeq) = {
		val obj = createUserdata(ground, seq)
		ground.send(this, getSymbol(ground, PSymbol(PNames.modifyInstructionSeq)), List(obj))
		obj
	}

	def createSend(ground: PObject, send: Send) = {
		val obj = createUserdata(ground, send)
		ground.send(this, getSymbol(ground, PSymbol(PNames.modifySend)), List(obj))
		obj
	}

	def createRawFunction(ground: PObject, fn: PRawFunction) = {
		val obj = createUserdata(ground, fn)
		ground.send(this, getSymbol(ground, PSymbol(PNames.modifyRawFunction)), List(obj))
		obj
	}

	def createCallGround(ground: PObject, fn: PRawFunction, send: Send) = {
		val obj = createObject(ground)
		obj.hints("fn") = fn.createObject(this)
		obj.hints("send") = send.createObject(this)
		ground.send(
			this,
			PNames.modifyCallGround,
			List(
				obj,
				createRawFunction(ground, fn),
				createSend(ground, send)
			)
		)
		obj
	}
}