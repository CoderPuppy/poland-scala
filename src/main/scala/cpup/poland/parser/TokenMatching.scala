package cpup.poland.parser

object TokenMatching {
	trait Matcher {
		def check(tok: Lexer.Token): Boolean
	}

	object MNone extends Matcher {
		def check(tok: Lexer.Token) = false
	}

	object MAll extends Matcher {
		def check(tok: Lexer.Token) = true
	}

	case class MToken(matchToken: Lexer.Token) extends Matcher {
		def check(tok: Lexer.Token) = tok == matchToken
	}

	case class MTokenType(matchType: Lexer.TokenType) extends Matcher {
		def check(tok: Lexer.Token) = tok.tokenType == matchType
	}

	case class MAnd(a: Matcher, b: Matcher) extends Matcher {
		def check(tok: Lexer.Token) = a.check(tok) && b.check(tok)
	}

	case class MOr(a: Matcher, b: Matcher) extends Matcher {
		def check(tok: Lexer.Token) = a.check(tok) || b.check(tok)
	}

	case class MNot(matcher: Matcher) extends Matcher {
		def check(tok: Lexer.Token) = !matcher.check(tok)
	}

	case class MMOr(matchers: Seq[Matcher]) extends Matcher {
		def check(tok: Lexer.Token) = matchers.exists(_.check(tok))
	}

	case class MMAnd(matchers: Seq[Matcher]) extends Matcher {
		def check(tok: Lexer.Token) = matchers.forall(_.check(tok))
	}

	def all = MAll
	def none = MNone

	def not(matcher: Matcher) = MNot(matcher)
	def token(tok: Lexer.Token) = MToken(tok)
	def tokenType(tokenType: Lexer.TokenType) = MTokenType(tokenType)

	def or(a: Matcher, b: Matcher) = MOr(a, b)
	def mor(matchers: Matcher*) = MMOr(matchers)

	def and(a: Matcher, b: Matcher) = MAnd(a, b)
	def mand(matchers: Matcher*) = MMAnd(matchers)
}