package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import cpup.poland.runtime.userdata.PSymbol
import cpup.poland.runtime.userdata.PString
import scala.util.Random

class PObject(val runtime: PRuntime) {
	final val objID = (getClass.getName, {
		val arr = new ListBuffer[Char]()
		for(i <- 0 until 12) {
			arr += (Random.nextInt(75).toChar + '0').toChar
		}
		arr.mkString("")
	}).toString

	def id = if(userdata == null) {
		objID
	} else {
		userdata.objID(this)
	}

	override def toString = toString(runtime.root)
	def toString(ground: PObject): String = toString(runtime, ground)
	def toString(runtime: PRuntime, ground: PObject) = if(userdata == null) {
		send(ground, runtime, "toString").userdata match {
			case PString(text) => text
			case _ => id
		}
	} else {
		userdata.toString
	}

	private var _userdata: Userdata = null
	def userdata = _userdata
	def userdata_=(newUserdata: Userdata) = {
		if(_userdata != null) {
			_userdata.cleanup(this)
		}
		_userdata = newUserdata
		newUserdata.init(this)
		newUserdata
	}

	val hints = new mutable.HashMap[String, PObject]()

	val slots = new mutable.HashMap[String, PObject]
	val sources = new ListBuffer[PObject]
	def derive = new PObject(runtime).deriveFrom(this)
	def deriveFrom(src: PObject) = {
		if(!sources.contains(src)) {
			sources += src
		}
		this
	}

	def apply(name: PObject): PObject = {
		this(name.id)
	}
	def update(name: PObject, newVal: PObject): PObject = {
		this(name.id) = newVal
	}

	def apply(name: Userdata): PObject = {
		this(name.objID(this))
	}
	def update(name: Userdata, newVal: PObject): PObject = {
		this(name.objID(this)) = newVal
	}

	def apply(name: String, path: List[String] = List()): PObject = {
		if(path.contains(objID)) {
			return runtime.nil
		}

		slots.getOrElse(name, {
			var value = runtime.nil

			Breaks.breakable {
				for(src <- sources) {
					val srcVal = src(name, path ++ List(objID))
					if(srcVal.userdata != PNil) { // TODO: Maybe !isInstanceOf[TNil]
						value = srcVal
						Breaks.break
					}
				}
			}

			value
		})
	}
	def update(name: String, newVal: PObject) = {
		slots(name) = newVal
		newVal
	}

	def receive(send: Send) = {
		val obj = this(send.msg.name)

		if(obj.isCallable(send.context.runtime, runtime.root)) {
			obj.call(send)
		} else { obj }
	}

	def send(root: PObject, runtime: PRuntime, name: PObject, args: PObject*) = {
		Send.fromObjs(runtime, root, this, name, args: _*).send
	}
	def send(root: PObject, runtime: PRuntime, name: String, args: PObject*): PObject = {
		send(root, runtime, runtime.getSymbol(root, name), args: _*)
	}

	def send(runtime: PRuntime, name: PObject, args: PObject*): PObject = {
		send(runtime.root, runtime, name, args: _*)
	}
	def send(runtime: PRuntime, name: String, args: PObject*): PObject = {
		send(runtime, runtime.getSymbol(runtime.root, name), args: _*)
	}

	def send(name: PObject, args: PObject*): PObject = {
		send(runtime, name, args: _*)
	}
	def send(name: String, args: PObject*): PObject = {
		send(runtime.getSymbol(runtime.root, name), args: _*)
	}

	def isCallable(runtime: PRuntime, ground: PObject) = if(userdata == null) {
		send(runtime, runtime.getSymbol(ground, PSymbol("isCallable"))).toBoolean(runtime, ground)
	} else {
		userdata.isCallable
	}
	def call(send: Send) = if(userdata == null) {
		this.send(
			runtime,
			runtime.getSymbol(runtime.root, PSymbol("call")),
			runtime.createSend(runtime.root, send)
		)
	} else {
		userdata.call(send)
	}

	def toBoolean(runtime: PRuntime, ground: PObject): Boolean = if(userdata == null || !userdata.isBoolean) {
		send(runtime, runtime.getSymbol(ground, PSymbol("toBoolean"))).toBoolean(runtime, ground)
	} else {
		userdata.toBoolean
	}
}