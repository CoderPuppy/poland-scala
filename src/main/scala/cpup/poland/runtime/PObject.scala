package cpup.poland.runtime

import cpup.poland.runtime.userdata.{PSymbol, Send, Userdata}
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
		userdata.objID
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

	def apply(runtime: TRuntime, name: PObject): PObject = {
		this(runtime, name.id)
	}
	def update(name: PObject, newVal: PObject): PObject = {
		this(name.id) = newVal
	}

	def apply(runtime: TRuntime, name: Userdata): PObject = {
		this(runtime, userdata.objID)
	}
	def update(name: Userdata, newVal: PObject): PObject = {
		this(name.objID) = newVal
	}

	def apply(runtime: TRuntime, name: String) = {
		slots.getOrElse(name, runtime.nil)
	}
	def update(name: String, newVal: PObject) = {
		slots(name) = newVal
		newVal
	}

	def receive(send: Send) = {
		val obj = this(send.runtime, send.msg.name)

		if(obj.isCallable(send.runtime)) {
			obj.call(send)
		} else { obj }
	}
	def send(root: PObject, runtime: PRuntime, name: PObject, args: PObject*) = {
		Send.fromObjs(runtime, root, this, name, args: _*).send
	}
	def send(runtime: PRuntime, name: PObject, args: PObject*): PObject = {
		send(runtime.root, runtime, name, args: _*)
	}

	def isCallable(runtime: PRuntime) = if(userdata == null) {
		send(runtime, runtime.getSymbol(PSymbol("isCallable"))).toBoolean(runtime)
	} else {
		userdata.isCallable
	}
	def call(send: Send) = if(userdata == null) {
		this.send(send.runtime, send.runtime.getSymbol(PSymbol("call")), send.runtime.createSend(send))
	} else {
		userdata.call(send)
	}

	def toBoolean(runtime: PRuntime): Boolean = if(userdata == null) {
		send(runtime, runtime.getSymbol(PSymbol("toBoolean"))).toBoolean(runtime)
	} else {
		userdata.toBoolean
	}
}