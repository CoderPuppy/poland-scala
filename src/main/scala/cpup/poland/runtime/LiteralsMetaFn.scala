package cpup.poland.runtime

import cpup.poland.runtime.userdata.{PString, PSymbol, Send}

case class LiteralsMetaFn(fallback: (Send, PObject) => Option[PObject]) extends ((Send, PObject) => Option[PObject]) {
	override def apply(send: Send, self: PObject) = {
//		println("name", send.msg.name)
		send.msg.name.userdata match {
			case PSymbol(str) if str.matches("^[\\d\\.]+$") =>
				println(str)
				Some(send.context.ground.runtime.createNumber(send.context.ground, str.toDouble))

			case PSymbol(symTxt) if symTxt.startsWith("\"") && symTxt.endsWith("\"") =>
				Some(getString(send, symTxt))
			case PSymbol(symTxt) if symTxt.startsWith("'") && symTxt.endsWith("'") =>
				Some(getString(send, symTxt))

			case _ => fallback(send, self)
		}
	}

	def getString(send: Send, symTxt: String) = {
		send.context.runtime.createString(
			send.context.ground,
			PString(
				symTxt.substring(1, symTxt.length - 1)
					.replaceAllLiterally("\\n", "\n")
					.replaceAllLiterally("\\r", "\r")
					.replaceAllLiterally("\\t", "\t")
					.replaceAllLiterally("\\\\", "\\")
					.replaceAllLiterally("\\'", "'")
					.replaceAllLiterally("\\\"", "\"")
			)
		)
	}
}