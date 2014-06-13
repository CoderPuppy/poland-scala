package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
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
	def toString(ground: PObject) = if(userdata == null) {
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

	val hints = new mutable.HashMap[String, Any]()

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

	type MetaFn = (Send, PObject) => Option[PObject]

	var metaFn: MetaFn = null

	def receive(send: Send) = receiveImpl(send).getOrElse(send.context.runtime.nil)
	def receiveImpl(send: Send): Option[PObject] = if(metaFn == null) {
		var res: Option[PObject] = None
		Breaks.breakable {
			for(src <- sources) {
				src.receiveImpl(send).foreach((obj) => {
					res = Some(obj)
					Breaks.break
				})
			}
		}
		res
	} else {
		metaFn(send, this)
	}

	def send(ground: PObject, name: PObject, args: Seq[PObject]) = {
		Send.fromObjs(ground, this, name, args: _*).send
	}
	def send(ground: PObject, name: String, args: PObject*): PObject = {
		// I'm using ground here because that's what you'd expect for syntactic sugar
		send(ground, runtime.getSymbol(ground, name), args)
	}

	def send(name: PObject, args: PObject*): PObject = {
		send(runtime.root, name, args)
	}
	def send(name: String, args: PObject*): PObject = {
		send(runtime.root, name, args: _*)
	}

	def isActivatable = if(userdata == null) {
		send(PNames.isActivatable).toBoolean
	} else {
		userdata.isActivatable
	}
	def activate(send: Send) = if(userdata == null) {
		this.send(
			send.context.ground,
			PNames.call,
			runtime.createSend(runtime.root, send) // TODO: Should I use ground here?
		)
	} else {
		userdata.activate(send)
	}

	def toBoolean: Boolean = if(userdata == null || !userdata.isBoolean) {
		send(PNames.toBoolean).toBoolean
	} else {
		userdata.toBoolean
	}
}