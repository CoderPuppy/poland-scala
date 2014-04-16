package cpup.poland

import cpup.poland.parser.{Parser, Lexer}
import cpup.poland.runtime.{PObject, PRuntime, BaseRuntime}

object TestPoland {
	def main(args: Array[String]) {
		println("Testing Poland (Scala Runtime)")
		val runtime = new PRuntime(new PObject)
		val tokens = Lexer.lex("println(\"Hello, World!\")", "test.pd")
		val seq = Parser.parse(runtime, tokens)
		println(s"Tokens: ${tokens}")
		println(s"Seq: ${seq}")
		println("Test Completed")
	}
}