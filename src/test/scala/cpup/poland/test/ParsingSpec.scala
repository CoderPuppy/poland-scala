package cpup.poland.test

import org.scalatest.{BeforeAndAfter, FreeSpec}
import cpup.poland.runtime.VanillaRuntime
import cpup.poland.parser.{Parser, Lexer}
import cpup.poland.runtime.userdata.{PSymbol, Message, InstructionSeq}

class ParsingSpec extends FreeSpec with BeforeAndAfter {
	var runtime: VanillaRuntime = null

	before {
		runtime = new VanillaRuntime
	}

	val context = "ParsingSpec"
	def lex(text: String) = Lexer.lex(text, context)
	def parse(text: String): InstructionSeq = parse(lex(text))
	def parse(tokens: List[Lexer.Token]) = Parser.parse(runtime, runtime.Ground, tokens)
	def assertVal(expected: Any)(actual: Any) = {
		assertResult(expected)(actual)
		actual
	}
	def assertType[R](o: Any)(implicit manifest: Manifest[R]) = {
		assert(manifest.runtimeClass.isInstance(o))
		o.asInstanceOf[R]
	}

	"Lexing" - {
		"an identifier" - {
			"should return a single token" in {
				val tokens = lex("identifier")
				assertVal(1) { tokens.length }
			}

			"should return an ID" in {
				val token = lex("identifier")(0)
				assertVal(Lexer.TokenType.ID) { token.tokenType }
				assertVal("identifier") { token.text }
			}

			"should return the correct position" in {
				val token = lex("identifier")(0)
				assertVal(Lexer.TokenPos("ParsingSpec", 0, 0, 0)) { token.pos }
			}
		}
	}
	"Parsing" - {
		"an identifier" - {
			"should return a single instruction" in {
				val seq = parse("identifier")
				assertVal(1) { seq.length }
			}

			"should return a single parameter-less message" in {
				val seq = parse("identifier")
				val msg = assertType[Message](seq(0))
				val name = assertType[PSymbol](msg.name.userdata)
				assertVal("identifier") { name.text }
				assertVal(0) { msg.args.length }
			}
		}
		"an identifier with empty parens" - {
			"should return a single instruction" in {
				val seq = parse("identifier()")
				assertVal(1) { seq.length }
			}

			"should return a single parameter-less message" in {
				val msg = assertType[Message](parse("identifier()")(0))
				val name = assertType[PSymbol](msg.name.userdata)
				assertVal("identifier") { name.text }
				assertVal(0) { msg.args.length }
			}
		}
	}
}