package cpup.poland.parser

import scala.util.control.Breaks
import scala.collection.mutable.ListBuffer
import cpup.poland.parser.{CharMatching => P}

case class Lexer(context: String, sugar: Boolean = true) {
	var mode: Lexer.Mode = null
	val modes = {
		var modes = List(
			Lexer.MatcherMode(Lexer.TokenType.Comma, P.MChar(',')),
			Lexer.MatcherMode(Lexer.TokenType.ForwardSlash, P.MChar('/')),
			Lexer.MatcherMode(Lexer.TokenType.Backslash, P.MChar('\\')),
			Lexer.MatcherMode(Lexer.TokenType.LineCommentBegin, P.MChar('#')),
			Lexer.MatcherMode(Lexer.TokenType.Newline, P.MOr(
				P.MChar('\n'),
				P.MChar('\r')
			)),
			Lexer.MatcherMode(Lexer.TokenType.Whitespace, P.MOr(
				P.MChar('\t'),
				P.MChar(' ')
			)),
			Lexer.MatcherMode(Lexer.TokenType.OpenParen, P.MChar('(')),
			Lexer.MatcherMode(Lexer.TokenType.CloseParen, P.MChar(',')),
			Lexer.MatcherMode(Lexer.TokenType.DoubleQuote, P.MChar('"')),
			Lexer.MatcherMode(Lexer.TokenType.SingleQuote, P.MChar('\'')),
			Lexer.MatcherMode(Lexer.TokenType.Reset, P.MChar('.'))
		)

		if(sugar) {
			modes ++= List(
				Lexer.MatcherMode(Lexer.TokenType.OpenCurly, P.MChar('{')),
				Lexer.MatcherMode(Lexer.TokenType.CloseCurly, P.MChar('}')),
				Lexer.MatcherMode(Lexer.TokenType.OpenSquare, P.MChar('[')),
				Lexer.MatcherMode(Lexer.TokenType.CloseSquare, P.MChar(']'))
			)
		}

		var nonID = P.MMOr(List(
			P.MChar('\''),
			P.MChar('"'),
			P.MChar('#'),
			P.MChar('('),
			P.MChar(')'),
			P.MChar('\n'),
			P.MChar('\r'),
			P.MChar('\t'),
			P.MChar(' '),
			P.MChar('/'),
			P.MChar('\\'),
			P.MChar(',')
		))

		if(sugar) {
			nonID = P.MMOr(nonID.matchers ++ List(
				P.MChar('['),
				P.MChar(']'),
				P.MChar('{'),
				P.MChar('}')
			))
		}

		modes ++= List(Lexer.MatcherMode(Lexer.TokenType.ID, P.MNot(nonID)))

		modes
	}

	var offset = 0
	var row    = 0
	var column = 0

	def leave {
		if(mode != null) {
			mode.leave(this)
		}

		mode = null
	}

	def handle(char: Char) = {
		var processed = true

		if(mode == null || !mode.handle(this, char)) {
			leave

			processed = false

			Breaks.breakable {
				for(modeClass <- modes) {
					val m = modeClass.dup
					mode = m
					m.enter(this)
					if(m.handle(this, char)) {
						processed = true
						Breaks.break
					} else {
						mode = null
					}
				}
			}
		}

		offset += 1
		column += 1
		if(char == '\n' || char == '\r') {
			row += 1
			column = 1
		}

		processed
	}

	val handlers = new ListBuffer[(Lexer.Token) => Unit]
	val tokens = new ListBuffer[Lexer.Token]

	def emit(token: Lexer.Token) {
		tokens += token
		for(handler <- handlers) {
			try {
				handler(token)
			} catch {
				case e: Throwable =>
					// TODO: Logging
					e.printStackTrace
			}
		}
	}
}

object Lexer {
	def lex(str: String, context: String = "runtime", sugar: Boolean = true) = {
		val lexer = Lexer(context, sugar)
		for(char <- str) {
			lexer.handle(char)
		}
		lexer.tokens.toList
	}

	case class Token(tokenType: TokenType, text: String, pos: TokenPos) {
		def withType(tokenType: TokenType) = Token(tokenType, text, pos)
		def withText(text:         String) = Token(tokenType, text, pos)
		def withPos (pos:        TokenPos) = Token(tokenType, text, pos)
	}
	case class TokenPos(context: String, char: Int, line: Int, column: Int) {
		def withContext(context: String) = TokenPos(context, char, line, column)
		def withChar   (char:       Int) = TokenPos(context, char, line, column)
		def withRow    (row:        Int) = TokenPos(context, char, row, column)
		def withColumn (column:     Int) = TokenPos(context, char, line, column)
	}
	sealed trait TokenType {}
	object TokenType {
		case object ID extends TokenType
		case object SingleQuote extends TokenType
		case object DoubleQuote extends TokenType

		case object Comma extends TokenType
		case object Reset extends TokenType

		case object LineCommentBegin extends TokenType
		case object ForwardSlash extends TokenType
		case object Backslash extends TokenType

		case object Newline extends TokenType
		case object Whitespace extends TokenType

		case object OpenParen extends TokenType
		case object CloseParen extends TokenType

		case object OpenCurly extends TokenType
		case object CloseCurly extends TokenType

		case object OpenSquare extends TokenType
		case object CloseSquare extends TokenType
	}

	sealed trait Mode {
		def enter(lexer: Lexer) {}
		def handle(lexer: Lexer, char: Char): Boolean
		def leave(lexer: Lexer) {}
		def dup: Mode
	}

	sealed trait MultiCharMode extends Mode {
		def max = Int.MaxValue

		def tokenType: TokenType
		var data = ""

		def check(lexer: Lexer, c: Char): Boolean

		var offset: Int = 0
		var row: Int    = 0
		var column: Int = 0

		def handle(lexer: Lexer, c: Char) = if(check(lexer, c)) {
			data += c.toString

			if(data.length == max) {
				lexer.leave
			}

			true
		} else {
			false
		}

		override def enter(lexer: Lexer) {
			offset = lexer.offset
			row = lexer.row
			column = lexer.column
		}

		override def leave(lexer: Lexer) {
			lexer.emit(Token(tokenType, data, TokenPos(lexer.context, offset, row, column)))
		}
	}

	case class MatcherMode(tokenType: TokenType, matcher: P.Matcher) extends MultiCharMode {
		def check(lexer: Lexer, c: Char) = matcher.check(c)
		def dup = MatcherMode(tokenType, matcher)
	}
}