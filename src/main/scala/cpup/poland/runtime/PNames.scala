package cpup.poland.runtime

object PNames {
	def reset = "." // TODO: is this really necessary? :slightlyannoyedpuppyface
	def newLine = "internal:new-line"
	def endSeq = "internal:end-seq"

	def symbols = "internal:symbols"

	def modifyObject = "internal:modify-object"
	def modifyUserdata = "internal:modify-userdata"
	def modifySymbol = "internal:modify-symbol"
	def modifyString = "internal:modify-string"
	def modifyNumber = "internal:modify-number"
	def modifyMessage = "internal:modify-message"
	def modifyInstructionSeq = "internal:modify-instruction-seq"
	def modifySend = "internal:modify-send"
	def modifyRawFunction = "internal:modify-raw-function"
	def modifyCallGround = "internal:modify-call-ground"

	def nil = "nil"
	def ptrue = "true"
	def pfalse = "false"
	def call = "call"
	def isActivatable = "is-activatable?"
	def toBoolean = "to-boolean" // TODO: to-bool?
}