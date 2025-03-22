local vm = require("@lute/vm")

local threadCount = tonumber(...)

for i = 1, threadCount do
	coroutine.resume(coroutine.create(vm.create("./serve").serve))
end

print(`Created {threadCount} server threads`)
