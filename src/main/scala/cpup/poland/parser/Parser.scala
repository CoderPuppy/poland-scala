package cpup.poland.parser

import cpup.poland.runtime.userdata.{Message, MessageSeq, PSymbol, PString}
import scala.collection.mutable
import cpup.poland.parser.{TokenMatching => M}

class Parser {
	var seq = new MessageSeq
	val stack = new mutable.Stack[Parser.Mode]

	def mode = if(stack.isEmpty) null else stack.head

	enter(new Parser.BodyMode(seq))

	def enter(newMode: Parser.Mode) {
		val oldMode = mode

		stack.push(newMode)

		if(oldMode == null) {
			mode.enter(this)
		} else {
			if(oldMode.enterChild(this, newMode)) {
				newMode.enter(this)
			} else {
				stack.pop
			}
		}
	}

	def leave {
		if(!stack.isEmpty) {
			val oldMode = stack.pop

			oldMode.leave(this)

			if(!stack.isEmpty) {
				mode.leaveChild(this, oldMode)
			}
		}
	}

	def handle(token: Lexer.Token) = if(mode == null) {
		false
	} else {
		mode.handle(this, token)
	}

	def done {
		while(!stack.isEmpty) {
			leave
		}
	}
}

object Parser {
	def parse(tokens: Seq[Lexer.Token]) = {
		val parser = new Parser
		for(tok <- tokens) {
			parser.handle(tok)
		}
		parser.done
		parser.seq
	}

	sealed trait Mode {
		def enter(parser: Parser) {}

		def enterChild(parser: Parser, child: Mode) = true
		def handle(parser: Parser, token: Lexer.Token): Boolean
		def leaveChild(parser: Parser, child: Mode) {}

		def leave(parser: Parser) {}
	}

	case class BodyMode(seq: MessageSeq, stopOn: M.Matcher = M.none) extends Mode {
		var lastWasSlash = false
		var msgPossible = true

		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			if(stopOn.check(tok)) {
				parser.leave
				return parser.handle(tok)
			}

			tok.tokenType match {
				case Lexer.TokenType.ID =>
					if(msgPossible) {
						parser.enter(MsgMode(Message(PSymbol(tok.text))))
					} else {
						throw ParseException("expected: whitespace, got: id, at line #{tok line}, column #{tok column}")
					}
					msgPossible = false

				case Lexer.TokenType.Whitespace =>
					msgPossible = true

				case Lexer.TokenType.Newline =>
					seq.add(Message(PSymbol(".")))
					msgPossible = true

				case Lexer.TokenType.Reset =>
					if(msgPossible) {
						parser.enter(MsgMode(Message(PSymbol("."))))
					} else {
						seq.add(Message(PSymbol(".")))
						msgPossible = true
					}

				case Lexer.TokenType.CloseParen =>
					parser.enter(new MsgMode(Message(PSymbol("apply")), false))
					return parser.handle(tok)

				case Lexer.TokenType.LineCommentBegin =>
					if(lastWasSlash) {
						lastWasSlash = false

						parser.enter(new BlockCommentMode)
					} else {
						parser.enter(new LineCommentMode)
					}

				case Lexer.TokenType.ForwardSlash =>
					// TODO: backtracking
					lastWasSlash = true

				case Lexer.TokenType.DoubleQuote =>
					parser enter StringMode(Lexer.TokenType.DoubleQuote)

				case Lexer.TokenType.SingleQuote =>
					parser.enter(StringMode(Lexer.TokenType.SingleQuote))

				case Lexer.TokenType.OpenSquare =>
					parser enter WrapperMode(Lexer.TokenType.CloseSquare, "[]")

				case Lexer.TokenType.OpenCurly =>
					parser enter WrapperMode(Lexer.TokenType.CloseCurly, "{}")

				case _ =>
					parser.leave
					return parser.handle(tok)
			}

			if(lastWasSlash && tok.tokenType != Lexer.TokenType.LineCommentBegin) {
				throw ParseException(s"unexpected fwdslash at line ${tok.pos.row - 1} column ${tok.pos.column} (char ${tok.pos.char - 1})")
			}

			true
		}

		override def enterChild(parser: Parser, child: Mode): Boolean = {
			child match {
				case mode: MsgMode =>
					if(msgPossible) {
						seq.add(mode.msg)
					} else {
						return false
					}
				case _ =>
			}

			true
		}

		override def leaveChild(parser: Parser, child: Mode){
			child match {
				case mode: StringMode =>
					parser enter MsgMode(Message(PString(mode.str)))

				case _ =>
			}
		}
	}

	case class MsgMode(msg: Message) extends Mode {
		def this(msg: Message, _inName: Boolean) {
			this(msg)
			inName = _inName
		}

		var inName = true
		var inArgs = false
		var lastWasSlash = false

		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			tok.tokenType match {
				case Lexer.TokenType.ID =>
					if(inName && msg.name.isInstanceOf[PSymbol]) {
						msg.name = PSymbol(msg.name.asInstanceOf[PSymbol].text + tok.text)
					}

				case Lexer.TokenType.Whitespace =>
					if(!inArgs) {
						parser.leave
						return parser.handle(tok)
					} else {
						inName = false
					}

				case Lexer.TokenType.Newline =>
					if(inName) {
						parser.leave
						return parser.handle(tok)
					}

				case Lexer.TokenType.LineCommentBegin =>
					if(inArgs) {
						if(lastWasSlash) {
							lastWasSlash = false
							parser.enter(new BlockCommentMode)
						} else {
							parser.enter(new LineCommentMode)
						}
					} else {
						parser.leave

						var good = true

						if(lastWasSlash) {
							lastWasSlash = false
							good = good && parser.handle(Lexer.Token(
								Lexer.TokenType.ForwardSlash, "/",
								Lexer.TokenPos(tok.pos.context, tok.pos.char - 1, tok.pos.row, tok.pos.column - 1)
							))
						}

						if(good) {
							return parser.handle(tok)
						} else {
							return false
						}
					}

				case Lexer.TokenType.ForwardSlash =>
					lastWasSlash = true

				case Lexer.TokenType.OpenParen =>
					inArgs = true
					parser.enter(ListMode(msg.args, M.tokenType(Lexer.TokenType.CloseParen)))

				case Lexer.TokenType.CloseParen =>
					parser.leave
					if(!inArgs) {
						return parser.handle(tok)
					}

				case _ =>
					parser.leave
					return parser.handle(tok)
			}

			true
		}

		override def enter(parser: Parser) {
			if(msg.name.isInstanceOf[PString]) {
				inName = false
			}
		}
	}

	class LineCommentMode extends Mode {
		def handle(parser: Parser, tok: Lexer.Token) = {
			tok.tokenType match {
				case Lexer.TokenType.Newline => parser.leave

				case _ =>
			}

			true
		}
	}

	class BlockCommentMode extends Mode {
		var lastWasComment = false

		def handle(parser: Parser, tok: Lexer.Token) = {
			tok.tokenType match {
				case Lexer.TokenType.LineCommentBegin =>
					lastWasComment = true

				case Lexer.TokenType.ForwardSlash =>
					if(lastWasComment) {
						parser.leave
					}

				case _ =>
			}

			true
		}
	}

	case class StringMode(tokenType: Lexer.TokenType, var str: String = "") extends Mode {
		def handle(parser: Parser, tok: Lexer.Token) = {
			tok.tokenType match {
				case `tokenType` =>
					parser.leave

				case _ =>
					str += tok.text
			}

			true
		}
	}

	case class ListMode(list: mutable.Buffer[MessageSeq], stopOn: M.Matcher = M.MNone) extends Mode {
		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			if(stopOn.check(tok)) {
				parser.leave
				return parser.handle(tok)
			}

			tok.tokenType match {
				case Lexer.TokenType.Whitespace =>

				case Lexer.TokenType.Comma =>
					val seq = new MessageSeq
					list += seq
					parser enter BodyMode(seq, M.MOr(stopOn, M.MTokenType(Lexer.TokenType.Comma)))

				case _ =>
					if(list.size == 0) {
						val seq = new MessageSeq
						list += seq
						parser.enter(BodyMode(seq, M.MOr(stopOn, M.MTokenType(Lexer.TokenType.Comma))))
						return parser.handle(tok)
					} else {
						throw ParseException("expected: comma, got: #{tok type}")
					}
			}

			true
		}
	}

	case class WrapperMode(close: Lexer.TokenType, var id: String) extends Mode {
		val msg = Message(PSymbol(id))
		val args = msg.args

		var first = true

		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			tok.tokenType match {
				case `close` =>
					if(first) {
						// TODO: this is very specific to being used in a body
						parser.leave
						parser.enter(MsgMode(msg))
					}

				case _ =>
					parser enter ListMode(args, M.MTokenType(close))
					return parser.handle(tok)
			}

			first = false

			true
		}
	}
}

case class ParseException(msg: String) extends Exception(msg)