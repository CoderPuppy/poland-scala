package cpup.poland.runtime

import cpup.poland.runtime.userdata._
import scala.collection.mutable

trait TRuntime {
	def root: PObject
	def nil: PObject

	def createSymbol(ground: PObject, sym: PSymbol): PObject

	def getSymbol(ground: PObject, sym: PSymbol) = ground.symbols.getOrElseUpdate(sym.text, createSymbol(ground, sym))
	def getSymbol(ground: PObject, sym: String): PObject = getSymbol(ground, PSymbol(sym))

	def createString(ground: PObject, str: PString): PObject
	def createMessage(ground: PObject, msg: Message): PObject
	def createMessageSeq(ground: PObject, seq: MessageSeq): PObject
	def createSend(ground: PObject, send: Send): PObject
}