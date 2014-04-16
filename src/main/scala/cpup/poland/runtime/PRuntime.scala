package cpup.poland.runtime

import cpup.poland.runtime.userdata.{Send, PSymbol}

case class PRuntime(root: PObject) extends TRuntime with TBaseRuntime {
	var createSymbolName: PObject = null
	override def createSymbol(sym: PSymbol) = {
		if(createSymbolName == null) {
			createSymbolName = PSymbol("createSymbol").createObject
		}

		val obj = Send(root, createSymbolName).send
		obj.userdata = sym

		obj.userdata match {
			case PSymbol(text) if text == "createSymbol" =>
				createSymbolName = obj
				
			case _ =>
		}

		obj
	}
}