package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import scala.collection.mutable

trait TRuntime {
	def nil: PObject

	def createSymbol(sym: PSymbol): PObject

	val symbols = new mutable.HashMap[String, PObject]()
	def getSymbol(sym: PSymbol) = symbols.getOrElseUpdate(sym.text, createSymbol(sym))
	def getSymbol(sym: String): PObject = getSymbol(PSymbol(sym))

	def createString(str: PString): PObject
	def createMessage(msg: Message): PObject
	def createMessageSeq(seq: MessageSeq): PObject
	def createSend(send: Send): PObject
}