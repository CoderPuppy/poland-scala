package cpup.poland.runtime.userdata

import cpup.poland.runtime.{PObject, PRuntime}

case class SendContext(runtime: PRuntime, ground: PObject, receiver: PObject) extends Userdata