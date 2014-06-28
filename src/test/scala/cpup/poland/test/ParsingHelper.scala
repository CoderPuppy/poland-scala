package cpup.poland.test

import cpup.poland.runtime.userdata.{PSymbol, Message, TInstruction, InstructionSeq}
import cpup.poland.parser.{Parser, Lexer}

trait ParsingHelper extends Helper with PolandHelper with LexingHelper {
	def parse(text: String): InstructionSeq = parse(lex(text))
	def parse(tokens: List[Lexer.Token]) = Parser.parse(runtime, runtime.Ground, tokens)

	def assertMessage(instr: TInstruction, name: String, args: Int, fn: (Message) => Unit = (msg) => ()) {
		val msg = assertType[Message](instr)
		assertResult(name) {
			assertType[PSymbol](msg.name.userdata).text
		}
		assertResult(args) { msg.args.length }
	}
}