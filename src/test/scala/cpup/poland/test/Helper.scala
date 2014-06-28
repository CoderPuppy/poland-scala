package cpup.poland.test

import org.scalatest.Suite

trait Helper extends Suite {
	// currently unused
//	def assertVal(expected: Any)(actual: Any) = {
//		super.assertResult(expected)(actual)
//		actual
//	}

	def assertType[R](o: Any)(implicit manifest: Manifest[R]) = {
		assert(manifest.runtimeClass.isInstance(o))
		o.asInstanceOf[R]
	}
}