package cpup.poland

import cpup.poland.parser.{Parser, Lexer}
import cpup.poland.runtime.{PObject, PRuntime, BaseRuntime}

object TestPoland {
	def main(args: Array[String]) {
		println("Testing Poland (Scala Runtime)")
		val runtime = new PRuntime(new PObject)
		println(Parser.parse(runtime, Lexer.lex("println(\"Hello, World!\")", "test.pd")))
		println("Test Completed")
	}
}