package cpup.poland.runtime

import cpup.poland.runtime.userdata.Userdata
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

	def apply(name: PObject) = slots(name.id)
	def update(name: PObject, newVal: PObject) = { slots(name.id) = newVal; newVal }
	def apply(name: Userdata) = slots(name.id)
	def update(name: Userdata, newVal: PObject) = { slots(name.id) = newVal; newVal }
	def apply(name: String) = slots(name)
	def update(name: String, newVal: PObject) = { slots(name) = newVal; newVal }
}