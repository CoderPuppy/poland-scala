package cpup.poland

import cpup.poland.parser.{Parser, Lexer}
import cpup.poland.runtime.RootGround
import cpup.poland.runtime.userdata.SendContext

object TestPoland {
	def main(args: Array[String]) {
		println("Testing Poland (Scala Runtime)")

		val (runtime, ground) = RootGround.create
		println(s"Ground: $ground")
		println(s"Runtime: $runtime")

		val tokens = Lexer.lex("println('Hello, World!' replace(/Hello/g, 'hi')", "test.pd")
		println(s"Tokens: $tokens")

		val seq = Parser.parse(runtime, ground, tokens)
		println(s"Seq: $seq")

		val result = seq.activate(SendContext(ground, ground))
		println(s"Result: $result")

		println("Test Completed")
	}
}