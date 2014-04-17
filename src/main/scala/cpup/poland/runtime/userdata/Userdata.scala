package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PRuntime, PObject}

trait Userdata {
	def id: String
	def objID = (getClass.getName, id).toString

	def init(obj: PObject) {}
	def cleanup(obj: PObject) {}

	def toString: String

	def isBoolean = false
	def toBoolean: Boolean = true

	def isCallable: Boolean = false
	def call(send: Send): PObject = {
		throw new RuntimeException("Attempt to call uncallable Userdata")
	}

	def createObject(runtime: PRuntime) = {
		val obj = new PObject(runtime)
		obj.userdata = this
		obj
	}
}