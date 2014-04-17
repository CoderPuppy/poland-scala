package cpup.poland

import cpup.poland.parser.{Parser, Lexer}
import cpup.poland.runtime.{Interpreter, RootGround, PObject, PRuntime}

object TestPoland {
	def main(args: Array[String]) {
		println("Testing Poland (Scala Runtime)")

		val ground = RootGround.create
		println(s"Ground: $ground")

		val runtime = PRuntime(ground)
		println(s"Runtime: $runtime")

		val tokens = Lexer.lex("println(\"Hello, World!\")", "test.pd")
		println(s"Tokens: $tokens")

		val seq = Parser.parse(runtime, tokens)
		println(s"Seq: $seq")

		val result = Interpreter.eval(runtime, ground, seq)
		println(s"Result: $result")

		println("Test Completed")
	}
}