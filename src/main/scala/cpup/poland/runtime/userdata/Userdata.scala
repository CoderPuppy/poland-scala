package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

trait Userdata {
	def id: String
	def toString: String
	def toBoolean: Boolean = true

	def isCallable: Boolean = false
	def call(send: Send): PObject = send.runtime.createNil(new PNil)

	def createObject = {
		val obj = new PObject
		obj.userdata = this
		obj
	}
}