const { readFile } = require('fs')

function spawn (source) {
  const shared = new SharedArrayBuffer(4)
  const u32 = new Uint32Array(shared)
  const tid = just.thread.spawn(source, just.args.slice(1), shared)
  const thread = { tid, u32 }
  threads.push(thread)
  return thread
}

function onTimer () {
  const { user, system } = just.cpuUsage()
  const { rss } = just.memoryUsage()
  let total = 0
  for (const thread of threads) {
    total += Atomics.load(thread.u32, 0)
  }
  just.error(`threads ${threads.length} total ${total} mem ${rss} cpu (${user.toFixed(2)}/${system.toFixed(2)}) ${(user + system).toFixed(2)} qps/core ${(total / (user + system)).toFixed(2)}`)
}

const source = readFile(just.args[2] || 'test.js')
const cpus = parseInt(just.env().CPUS || just.sys.cpus, 10)
const threads = []
for (let i = 0; i < cpus; i++) spawn(source)
just.setInterval(onTimer, 1000)
