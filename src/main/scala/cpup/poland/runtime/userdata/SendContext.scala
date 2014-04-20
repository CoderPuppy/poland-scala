package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PObject, PRuntime}

case class SendContext(ground: PObject, receiver: PObject) extends Userdata {
	val runtime = ground.runtime

	assert(runtime == receiver.runtime, "Ground and receiver are in different runtimes")

	def withReceiver(receiver: PObject) = SendContext(ground, receiver)
}