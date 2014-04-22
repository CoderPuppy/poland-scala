package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import cpup.poland.runtime.userdata.PSymbol
import cpup.poland.runtime.userdata.PString
import scala.util.Random

class PObject(val runtime: BaseRuntime) {
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
	def toString(runtime: BaseRuntime, ground: PObject) = if(userdata == null) {
		send(ground, "toString").userdata match {
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

	def send(ground: PObject, name: PObject, args: Seq[PObject]) = {
		Send.fromObjs(ground, this, name, args: _*).send
	}
	def send(ground: PObject, name: PObject): PObject = send(ground, name, List())
	def send(ground: PObject, name: String, args: Seq[PObject]): PObject = {
		send(ground, runtime.getSymbol(ground, name), args)
	}
	def send(ground: PObject, name: String): PObject = send(ground, name, List())

	def send(runtime: BaseRuntime, name: PObject, args: Seq[PObject]): PObject = {
		send(runtime.root, name, args)
	}
	def send(runtime: BaseRuntime, name: PObject): PObject = send(runtime, name, List())
	def send(runtime: BaseRuntime, name: String, args: Seq[PObject]): PObject = {
		send(runtime, runtime.getSymbol(runtime.root, name), args)
	}
	def send(runtime: BaseRuntime, name: String): PObject = send(runtime, name, List())

	def send(name: PObject, args: PObject*): PObject = {
		send(runtime, name, args)
	}
	def send(name: String, args: PObject*): PObject = {
		send(runtime.getSymbol(runtime.root, name), args: _*)
	}

	def isCallable(runtime: BaseRuntime, ground: PObject) = if(userdata == null) {
		send(runtime, runtime.getSymbol(ground, PSymbol("isCallable"))).toBoolean(runtime, ground)
	} else {
		userdata.isCallable
	}
	def call(send: Send) = if(userdata == null) {
		this.send(
			runtime,
			runtime.getSymbol(runtime.root, PSymbol("call")),
			List(
				runtime.createSend(runtime.root, send)
			)
		)
	} else {
		userdata.call(send)
	}

	def toBoolean(runtime: BaseRuntime, ground: PObject): Boolean = if(userdata == null || !userdata.isBoolean) {
		send(runtime, runtime.getSymbol(ground, PSymbol("toBoolean"))).toBoolean(runtime, ground)
	} else {
		userdata.toBoolean
	}
}