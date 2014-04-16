package cpup.poland.parser

object CharMatching {
	sealed trait Matcher {
		def check(c: Char): Boolean
	}

	object MNone extends Matcher {
		def check(c: Char) = false
	}

	object MAll extends Matcher {
		def check(c: Char) = true
	}

	case class MRange(min: Char, max: Char) extends Matcher {
		def check(c: Char) = c >= min && c <= max
	}

	case class MChar(char: Char) extends Matcher {
		def check(c: Char) = c == char
	}

	case class MAnd(a: Matcher, b: Matcher) extends Matcher {
		def check(c: Char) = a.check(c) && b.check(c)
	}

	case class MOr(a: Matcher, b: Matcher) extends Matcher {
		def check(c: Char) = a.check(c) || b.check(c)
	}

	case class MNot(matcher: Matcher) extends Matcher {
		def check(c: Char) = !matcher.check(c)
	}

	case class MMOr(matchers: Seq[Matcher]) extends Matcher {
		def check(c: Char) = matchers.exists(_.check(c))
	}

	case class MMAnd(matchers: Seq[Matcher]) extends Matcher {
		def check(c: Char) = matchers.forall(_.check(c))
	}

	def none = MNone
	def all = MAll

	def range(min: Char, max: Char) = MRange(min, max)
	def char(char: Char) = MChar(char)

	def and(a: Matcher, b: Matcher) = MAnd(a, b)
	def mand(matchers: Matcher*) = MMAnd(matchers)

	def or(a: Matcher, b: Matcher) = MOr(a, b)
	def mor(matchers: Matcher*) = MMOr(matchers)
}