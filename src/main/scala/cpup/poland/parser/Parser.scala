package cpup.poland.parser

import cpup.poland.runtime.userdata.{Message, InstructionSeq, PSymbol, PString}
import scala.collection.mutable
import cpup.poland.parser.{TokenMatching => M}
import cpup.poland.runtime.{PRuntime, PObject}

case class Parser(runtime: PRuntime, ground: PObject) {
	var seq = new InstructionSeq
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
	def parse(runtime: PRuntime, ground: PObject, tokens: Seq[Lexer.Token]) = {
		val parser = new Parser(runtime, ground)
		for(tok <- tokens) {
			parser.handle(tok)
		}
		parser.done
		parser.seq
	}
	def parse(runtime: PRuntime, tokens: Seq[Lexer.Token]): InstructionSeq = parse(runtime, runtime.root, tokens)

	sealed trait Mode {
		def enter(parser: Parser) {}

		def enterChild(parser: Parser, child: Mode) = true
		def handle(parser: Parser, token: Lexer.Token): Boolean
		def leaveChild(parser: Parser, child: Mode) {}

		def leave(parser: Parser) {}
	}

	case class BodyMode(seq: InstructionSeq, stopOn: M.Matcher = M.none) extends Mode {
		var lastWasSlash = false
		var lastSlash: Lexer.Token = null
		var ignoreNextSlash = false

		var msgPossible = true

		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			if(stopOn.check(tok)) {
				parser.leave
				return parser.handle(tok)
			}

			if(lastWasSlash && tok.tokenType != Lexer.TokenType.LineCommentBegin) {
				ignoreNextSlash = true
				lastWasSlash = false
				return parser.handle(lastSlash) && parser.handle(tok)
			}

			tok.tokenType match {
				case Lexer.TokenType.ID =>
					if(msgPossible) {
						parser.enter(MsgMode(Message(
							parser.runtime.getSymbol(parser.ground, PSymbol(tok.text)),
							tok.pos
						)))
					} else {
						throw ParseException(s"expected: Whitespace, got: ID, at line ${tok.pos.line}, column ${tok.pos.column}")
					}
					msgPossible = false

				case Lexer.TokenType.Whitespace =>
					msgPossible = true

				case Lexer.TokenType.Newline =>
					seq.add(Message(
						parser.runtime.getSymbol(parser.ground, PSymbol(".")),
						tok.pos
					))
					msgPossible = true

				case Lexer.TokenType.Reset =>
					if(msgPossible) {
						parser.enter(MsgMode(Message(
							parser.runtime.getSymbol(parser.ground, PSymbol(".")),
							tok.pos
						)))
					} else {
						seq.add(Message(
							parser.runtime.getSymbol(parser.ground, PSymbol(".")),
							tok.pos
						))
						msgPossible = true
					}

				case Lexer.TokenType.OpenParen =>
					parser.enter(new MsgMode(
						Message(
							parser.runtime.getSymbol(parser.ground, PSymbol("apply")),
							tok.pos
						),
						false
					))
					return parser.handle(tok)

				case Lexer.TokenType.LineCommentBegin =>
					if(lastWasSlash) {
						lastWasSlash = false

						parser.enter(new BlockCommentMode)
					} else {
						parser.enter(new LineCommentMode)
					}

				case Lexer.TokenType.ForwardSlash if ignoreNextSlash =>
					ignoreNextSlash = false
					if(msgPossible) {
						parser.enter(MsgMode(Message(
							parser.runtime.getSymbol(parser.ground, PSymbol(tok.text)),
							tok.pos
						)))
					} else {
						throw ParseException(s"expected: Whitespace, got: ForwardSlash, at line ${tok.pos.line}, column ${tok.pos.column}")
					}
					msgPossible = false

				case Lexer.TokenType.ForwardSlash if !ignoreNextSlash =>
					lastWasSlash = true
					lastSlash = tok

				case Lexer.TokenType.DoubleQuote | Lexer.TokenType.SingleQuote =>
					parser enter StringMode(tok)

				case Lexer.TokenType.OpenSquare =>
					parser enter WrapperMode(tok, Lexer.TokenType.CloseSquare, "[]")

				case Lexer.TokenType.OpenCurly =>
					parser enter WrapperMode(tok, Lexer.TokenType.CloseCurly, "{}")

				case _ =>
					parser.leave
					return parser.handle(tok)
			}

			true
		}

		override def leaveChild(parser: Parser, child: Mode){
			child match {
				case mode: StringMode =>
					parser enter MsgMode(Message(
						parser.runtime.createString(parser.ground, PString(mode.str)),
						mode.start.pos
					))

				case mode: MsgMode =>
					seq.add(mode.msg)

				case mode: WrapperMode =>
					seq.add(mode.msg)

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
		var lastSlash: Lexer.Token = null
		var ignoreNextSlash = false

		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			if(lastWasSlash && tok.tokenType != Lexer.TokenType.LineCommentBegin) {
				ignoreNextSlash = true
				lastWasSlash = false
				return parser.handle(lastSlash) && parser.handle(tok)
			}

			tok.tokenType match {
				case Lexer.TokenType.ID if inName =>
					msg.name.userdata match {
						case symbol: PSymbol =>
							msg.name = parser.runtime.getSymbol(
								parser.ground,
								PSymbol(symbol.text + tok.text)
							)
						case _ =>
					}

				case Lexer.TokenType.SingleQuote | Lexer.TokenType.DoubleQuote if inName =>
					parser.enter(StringMode(tok))

//				case Lexer.TokenType.Whitespace if inArgs =>
//					inName = false // TODO: hurah?

				case Lexer.TokenType.Whitespace if !inArgs =>
					parser.leave
					return parser.handle(tok)

				case Lexer.TokenType.Newline if inName =>
					parser.leave
					return parser.handle(tok)

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
								Lexer.TokenPos(tok.pos.context, tok.pos.char - 1, tok.pos.line, tok.pos.column - 1)
							))
						}

						if(good) {
							return parser.handle(tok)
						} else {
							return false
						}
					}

				case Lexer.TokenType.ForwardSlash if ignoreNextSlash && inName =>
					ignoreNextSlash = false
					msg.name.userdata match {
						case symbol: PSymbol =>
							msg.name = parser.runtime.getSymbol(
								parser.ground,
								PSymbol(symbol.text + tok.text)
							)
						case _ =>
					}

				case Lexer.TokenType.ForwardSlash if !ignoreNextSlash =>
					lastWasSlash = true
					lastSlash = tok

				case Lexer.TokenType.OpenParen =>
					inName = false
					inArgs = true
					parser.enter(ListMode(msg.args, M.tokenType(Lexer.TokenType.CloseParen)))

				case Lexer.TokenType.CloseParen =>
					if(inArgs) {
						parser.leave
					} else {
						throw ParseException("")
					}

				case _ =>
					parser.leave
					return parser.handle(tok)
			}

			true
		}

		override def enter(parser: Parser) {
			if(msg.name.userdata.isInstanceOf[PString]) {
				inName = false
			}
		}

		override def leaveChild(parser: Parser, child: Mode) {
			child match {
				case StringMode(tokenType, str) =>
					if(inName && msg.name.userdata.isInstanceOf[PSymbol]) {
						msg.name = parser.runtime.getSymbol(
							parser.ground,
							PSymbol(msg.name.userdata.asInstanceOf[PSymbol].text + str)
						)
					}

				case _ =>
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

	case class StringMode(start: Lexer.Token, var str: String = "") extends Mode {
		var lastWasBackslash = false
		final val tokenType = start.tokenType

		def handle(parser: Parser, tok: Lexer.Token) = {
			tok.tokenType match {
				case Lexer.TokenType.Backslash =>
					if(lastWasBackslash) {
						str += tok.text
					} else {
						lastWasBackslash = true
					}

				case `tokenType` =>
					if(!lastWasBackslash) {
						parser.leave
					}

				case _ =>
					str += tok.text
			}

			if(tok.tokenType != Lexer.TokenType.Backslash) {
				lastWasBackslash = false
			}

			true
		}
	}

	case class ListMode(list: mutable.Buffer[InstructionSeq], stopOn: M.Matcher = M.MNone) extends Mode {
		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			if(stopOn.check(tok)) {
				parser.leave
				return parser.handle(tok)
			}

			tok.tokenType match {
				case Lexer.TokenType.Whitespace =>

				case Lexer.TokenType.Comma =>
					val seq = new InstructionSeq
					list += seq
					parser enter BodyMode(seq, M.MOr(stopOn, M.MTokenType(Lexer.TokenType.Comma)))

				case _ =>
					if(list.size == 0) {
						val seq = new InstructionSeq
						list += seq
						parser.enter(BodyMode(seq, M.MOr(stopOn, M.MTokenType(Lexer.TokenType.Comma))))
						return parser.handle(tok)
					} else {
						throw ParseException(s"expected: comma, got: ${tok.tokenType}, at line: ${tok.pos.line}, column: ${tok.pos.column}, char: ${tok.pos.char}")
					}
			}

			true
		}
	}

	case class WrapperMode(start: Lexer.Token, close: Lexer.TokenType, id: String) extends Mode {
		var msg: Message = null

		var first = true

		override def enter(parser: Parser) {
			msg = Message(
				parser.runtime.getSymbol(parser.ground, PSymbol(id)),
				start.pos
			)
		}

		def handle(parser: Parser, tok: Lexer.Token): Boolean = {
			tok.tokenType match {
				case `close` =>
					if(first) {
						// TODO: this is very specific to being used in a body
						parser.leave
						parser.enter(MsgMode(msg))
					}
					parser.leave

				case _ =>
					parser.enter(ListMode(msg.args, M.MTokenType(close)))
					return parser.handle(tok)
			}

			first = false

			true
		}
	}
}

case class ParseException(msg: String) extends Exception(msg)