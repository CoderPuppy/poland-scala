package cpup.poland.runtime.userdata

import cpup.poland.runtime.PObject

trait Userdata {
	def id: String
	def toString: String
	def createObject = {
		val obj = new PObject
		obj.userdata = this
		obj
	}
}