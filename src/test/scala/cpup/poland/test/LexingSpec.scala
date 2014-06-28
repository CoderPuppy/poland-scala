package cpup.poland.test

import cpup.poland.parser.Lexer

class LexingSpec extends BaseSpec with LexingHelper {
	"Lexing" - {
		"an identifier" - {
			"should return a single ID" in {
				val tokens = lex("identifier")
				assertResult(1) { tokens.length }
				val token = tokens(0)
				assertResult(Lexer.TokenType.ID) { token.tokenType }
				assertResult("identifier") { token.text }
			}

			"should return the correct position" in {
				val token = lex("identifier")(0)
				assertResult(Lexer.TokenPos("ParsingSpec", 0, 0, 0)) { token.pos }
			}
		}
	}
}