package cpup.poland.test

import cpup.poland.runtime.VanillaRuntime
import org.scalatest.{Suite, BeforeAndAfter}

trait PolandHelper extends Suite with BeforeAndAfter {
	var runtime: VanillaRuntime = null

	before {
		runtime = new VanillaRuntime
	}
}