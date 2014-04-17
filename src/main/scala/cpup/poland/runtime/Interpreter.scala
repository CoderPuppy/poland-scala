package cpup.poland.runtime

import cpup.poland.runtime.userdata.{Send, MessageSeq}

object Interpreter {
	def eval(runtime: PRuntime, ground: PObject, seq: MessageSeq, _current: PObject) = {
		var current = _current

		for(msg <- seq) {
			current = Send(runtime, ground, current, seq, msg).send
		}

		current
	}

	def eval(runtime: PRuntime, ground: PObject, seq: MessageSeq): PObject = eval(runtime, ground, seq, ground)
	def eval(runtime: PRuntime, seq: MessageSeq, current: PObject): PObject = eval(runtime, runtime.root, seq, current)
	def eval(runtime: PRuntime, seq: MessageSeq): PObject = eval(runtime, runtime.root, seq)
}