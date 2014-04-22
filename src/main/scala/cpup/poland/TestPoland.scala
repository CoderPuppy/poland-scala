package cpup.poland

import cpup.poland.parser.{Parser, Lexer}
import cpup.poland.runtime.VanillaRuntime
import cpup.poland.runtime.userdata.SendContext

object TestPoland {
	def main(args: Array[String]) {
		println("Testing Poland (Scala Runtime)")

		val runtime = new VanillaRuntime
		println(s"Runtime: $runtime")

		val tokens = Lexer.lex("println([]('Hello,', 'World!') join(' ') replace(/Hello/g, 'hi')", "test.pd")
		println(s"Tokens: $tokens")

		val seq = Parser.parse(runtime, runtime.Ground, tokens)
		println(s"Seq: $seq")

		val result = seq.activate(SendContext(runtime.Ground, runtime.Ground))
		println(s"Result: $result")

		println("Test Completed")
	}
}