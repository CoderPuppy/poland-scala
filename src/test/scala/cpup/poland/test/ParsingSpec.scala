package cpup.poland.test

import cpup.poland.runtime.userdata.Message

class ParsingSpec extends BaseSpec with ParsingHelper {
	"Parsing" - {
		"an identifier" - {
			"should return a single parameter-less message" in {
				val seq = parse("identifier")
				assertResult(1) { seq.length }
				assertMessage(seq(0), "identifier", 0)
			}
		}

		"an identifier with empty parens" - {
			"should return a single parameter-less message" in {
				val seq = parse("identifier()")
				assertResult(1) { seq.length }
				assertMessage(seq(0), "identifier", 0)
			}
		}

		"an empty array" - {
			"should return a single parameter-less message" in {
				val seq = parse("[]")
				assertResult(1) { seq.length }
				assertMessage(seq(0), "[]", 0)
			}

			"should work with parens" in {
				val seq = parse("[]()")
				assertResult(1) { seq.length }
				assertMessage(seq(0), "[]", 0)
			}
		}

		"an array" - {
			"should pass the parameters on" in {
				val seq = parse("[ test ]")
				assertResult(1) { seq.length }
				assertMessage(seq(0), "[]", 1, (msg: Message) => {
					val seq = msg.args(0)
					assertResult(1) { seq.length }
					assertMessage(seq(0), "test", 0)
				})
			}

			"should accept multiple parameters" in {
				val seq = parse("[ foo, bar, baz ]")
				assertResult(1) { seq.length }
				assertMessage(seq(0), "[]", 3, (msg: Message) => {
					val seq1 = msg.args(0)
					assertResult(1) { seq1.length }
					assertMessage(seq1(0), "foo", 0)

					val seq2 = msg.args(1)
					assertResult(1) { seq2.length }
					assertMessage(seq(0), "bar", 0)

					val seq3 = msg.args(2)
					assertResult(1) { seq3.length }
					assertMessage(seq(0), "baz", 0)
				})
			}

			"should accept parameters with arguments" in {
				val seq = parse("[ foo(bar), baz ]")
				assertResult(1) { seq.length }
				assertMessage(seq(0), "[]", 2, (msg: Message) => {
					val seq1 = msg.args(0)
					assertResult(3) { seq1.length }
					assertMessage(seq1(0), "foo", 0, (msg: Message) => {
						val seq = msg.args(0)
						assertResult(1) { seq.length }
						assertMessage(seq(0), "bar", 0)
					})

					val seq2 = msg.args(1)
					assertResult(1) { seq2.length }
					assertMessage(seq2(0), "baz", 0)
				})
			}
		}
	}
}