package cpup.poland.test

import cpup.poland.parser.Lexer

trait LexingHelper {
	val context = getClass.getName
	def lex(text: String) = Lexer.lex(text, context)
}