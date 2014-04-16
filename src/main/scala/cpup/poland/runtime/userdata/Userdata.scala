package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

trait Userdata {
	def id: String
	def objID = (getClass.getName, id).toString
	def toString: String
	def toBoolean: Boolean = true

	def isCallable: Boolean = false
	def call(send: Send): PObject = {
		throw new RuntimeException("Attempt to call uncallable Userdata")
	}

	def createObject = {
		val obj = new PObject
		obj.userdata = this
		obj
	}
}