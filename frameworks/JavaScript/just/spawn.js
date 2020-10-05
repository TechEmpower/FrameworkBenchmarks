const { cwd, errno, strerror, spawn } = just.sys
const path = cwd()
const [...args] = just.args.slice(2)
const { socketpair, AF_UNIX, SOCK_STREAM } = just.net
function createPipe () {
  const fds = []
  const r = socketpair(AF_UNIX, SOCK_STREAM, fds)
  if (r !== 0) throw new Error(`socketpair ${r} errno ${errno()} : ${strerror(errno())}`)
  return fds
}

const cpus = parseInt(just.env().CPUS || just.sys.cpus, 10)
const pids = []
for (let i = 0; i < cpus; i++) {
  const stdin = createPipe()
  const stdout = createPipe()
  const stderr = createPipe()
  const pid = spawn('just', path, args, stdin[1], stdout[1], stderr[1])
  pids.push(pid)
}

const { readStat } = require('lib/monitor.js')
const last = { user: 0, system: 0 }
just.setInterval(() => {
  const stat = { user: 0, system: 0, rss: 0 }
  for (const pid of pids) {
    const { utime, stime, rssPages } = readStat(pid)
    const rss = Math.floor((rssPages * just.sys.pageSize) / (1024 * 1024))
    stat.rss += rss
    stat.user += utime
    stat.system += stime
  }
  const user = stat.user - last.user
  const system = stat.system - last.system
  last.user = stat.user
  last.system = stat.system
  just.print(`children ${pids.length} rss ${stat.rss} user ${user} system ${system} total ${user + system}`)
}, 1000)
