package cpup.poland.runtime

import cpup.poland.runtime.userdata.InstructionSeq

object Interpreter {
	def eval(runtime: PRuntime, ground: PObject, seq: InstructionSeq, _current: PObject) = {
		var current = _current

		for(msg <- seq) {
			current = msg.activate(runtime, ground, current, seq)
		}

		current
	}

	def eval(runtime: PRuntime, ground: PObject, seq: InstructionSeq): PObject = eval(runtime, ground, seq, ground)
	def eval(runtime: PRuntime, seq: InstructionSeq, current: PObject): PObject = eval(runtime, runtime.root, seq, current)
	def eval(runtime: PRuntime, seq: InstructionSeq): PObject = eval(runtime, runtime.root, seq)
}