package cpup.poland.runtime

import cpup.poland.runtime.userdata.Send

object BasicMetaFn extends ((Send, PObject) => Option[PObject]) {
	def apply(send: Send, self: PObject) = {
		val obj = self(send.msg.name)

		Some(if(obj.isActivatable) {
			obj.activate(send)
		} else { obj })
	}
}