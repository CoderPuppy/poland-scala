package cpup.poland.test

import cpup.poland.runtime.userdata.Message

class ParsingSpec extends BaseSpec with ParsingHelper {
	"Parsing" - {
		"an identifier" - {
			"should return a single parameter-less message" in {
				val seq = parse("identifier")
				assertMessage(seq(0), "identifier", 0)
			}

			"should return the end-of-sequence message" in {
				val seq = parse("identifier")
				assertEndSeq(seq)
			}
		}

		"an identifier with empty parens" - {
			"should return a single parameter-less message" in {
				val seq = parse("identifier()")
				assertMessage(seq(0), "identifier", 0)
			}
		}

		"an empty array" - {
			"should return a single parameter-less message" in {
				val seq = parse("[]")
				assertMessage(seq(0), "[]", 0)
			}

			"should work with parens" in {
				val seq = parse("[]()")
				assertMessage(seq(0), "[]", 0)
			}
		}

		"an array" - {
			"should pass the parameters on" in {
				val seq = parse("[ test ]")
				assertMessage(seq(0), "[]", 1, (msg: Message) => {
					val seq = msg.args(0)
					assertMessage(seq(0), "test", 0)
				})
			}

			"should accept multiple parameters" in {
				val seq = parse("[ foo, bar, baz ]")
				assertMessage(seq(0), "[]", 3, (msg: Message) => {
					assertMessage(msg.args(0)(0), "foo", 0)
					assertMessage(msg.args(1)(0), "bar", 0)
					assertMessage(msg.args(2)(0), "baz", 0)
				})
			}

			"should accept parameters with arguments" in {
				val seq = parse("[ foo(bar), baz ]")
				assertMessage(seq(0), "[]", 2, (msg: Message) => {
					assertMessage(msg.args(0)(0), "foo", 1, (msg: Message) => {
						assertMessage(msg.args(0)(0), "bar", 0)
					})
					assertMessage(msg.args(1)(0), "baz", 0)
				})
			}
		}

		"nothing" - {
			"should be mostly empty" in {
				// TODO: this is bad because it knows about another feature
				val seq = parse("")
				assertResult(1) { seq.length }
			}

			"should contain the end of sequence message" in {
				val seq = parse("")
				assertEndSeq(seq)
			}
		}
	}
}