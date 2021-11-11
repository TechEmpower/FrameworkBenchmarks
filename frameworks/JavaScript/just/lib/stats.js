function start () {
  function onTime () {
    stats.time = (new Date()).toUTCString()
  }
  const stats = { conn: 0, qps: 0, rps: 0, time: 0 }
  just.setInterval(() => {
    if (just.buffer) {
      u32 = new Uint32Array(just.buffer)
    }
    if (u32) {
      Atomics.exchange(u32, 0, stats.rps)
    } else {
      const { conn, qps, rps, clients } = stats
      const { user, system } = just.cpuUsage()
      const { rss } = just.memoryUsage()
      const rpspc = ((rps / (user + system)) || 0)
      just.error(`conn ${conn} qps ${qps} rps ${rps} clients ${clients} mem ${rss} cpu (${user.toFixed(2)}/${system.toFixed(2)}) ${(user + system).toFixed(2)} rps/core ${rpspc.toFixed(2)}`)
    }
    stats.qps = stats.rps = 0
  }, 1000)
  just.setInterval(onTime, 100)
  onTime()
  return stats
}

let u32

module.exports = { start }
