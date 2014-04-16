package cpup.poland

import cpup.poland.parser.{Parser, Lexer}

object TestPoland {
	def main(args: Array[String]) {
		println("Testing Poland (Scala Runtime)")
		println(Parser.parse(Lexer.lex("println(\"Hello, World!\")", "test.pd")))
		println("Test Completed")
	}
}