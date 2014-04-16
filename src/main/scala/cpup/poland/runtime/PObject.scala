package cpup.poland.runtime

import cpup.poland.runtime.userdata.{PNil, PSymbol, Send, Userdata}
import scala.collection.mutable
import scala.util.Random
import scala.collection.mutable.ListBuffer

class PObject {
	val _id = {
		val arr = new ListBuffer[Char]()
		for(i <- 0 until 12) {
			arr += (Random.nextInt(75).toChar + '0').toChar
		}
		arr.mkString("")
	}

	def id = if(userdata == null) {
		_id
	} else {
		(userdata.getClass.getCanonicalName, userdata.id).toString
	}
	override def toString = if(userdata == null) {
		// TODO: send
		_id
	} else {
		userdata.toString
	}

	var userdata: Userdata = null

	val slots = new mutable.HashMap[String, PObject]
	val sources = new ListBuffer[PObject]
	def derive = new PObject().deriveFrom(this)
	def deriveFrom(src: PObject) = {
		if(!sources.contains(src)) {
			sources += src
		}
		this
	}

	def apply(runtime: TRuntime, name: PObject) = slots.getOrElse(name.id, runtime.createNil(new PNil))
	def update(name: PObject, newVal: PObject) = { slots(name.id) = newVal; newVal }
	def apply(runtime: TRuntime, name: Userdata) = slots.getOrElse(name.id, runtime.createNil(new PNil))
	def update(name: Userdata, newVal: PObject) = { slots(name.id) = newVal; newVal }
	def apply(runtime: TRuntime, name: String) = slots.getOrElse(name, runtime.createNil(new PNil))
	def update(name: String, newVal: PObject) = { slots(name) = newVal; newVal }

	def receive(send: Send) = {
		val obj = this(send.runtime, send.msg.name)

		if(obj.isCallable(send.runtime)) {
			obj.call(send)
		} else { obj }
	}
	def send(root: PObject, name: PObject, args: PObject*) = Send(root, this, name, args: _*).send

	def isCallable(runtime: PRuntime) = if(userdata == null) {
		send(runtime.root, runtime.createSymbol(PSymbol("isCallable"))).toBoolean(runtime)
	} else {
		userdata.isCallable
	}
	def call(send: Send) = if(userdata == null) {
		this.send(send.runtime.createSymbol(PSymbol("call")), send.runtime.createSend(send))
	} else {
		userdata.call(send)
	}

	def toBoolean(runtime: PRuntime): Boolean = if(userdata == null) {
		send(runtime.root, runtime.createSymbol(PSymbol("toBoolean"))).toBoolean(runtime)
	} else {
		userdata.toBoolean
	}
}