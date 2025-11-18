local vm = require("@lute/vm")
local system = require("@lute/system")

local threadCount = system.threadcount()

for _ = 1, threadCount do
	coroutine.resume(coroutine.create(vm.create("./serve").serve))
end

print(`Created {threadCount} server threads`)
