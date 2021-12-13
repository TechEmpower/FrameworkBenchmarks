const { fs, sys, net } = just

function readStat (pid = sys.pid()) {
  const buf = new ArrayBuffer(4096)
  const path = `/proc/${pid}/stat`
  const fd = fs.open(path)
  net.seek(fd, 0, net.SEEK_SET)
  let bytes = net.read(fd, buf)
  const parts = []
  while (bytes > 0) {
    parts.push(buf.readString(bytes))
    bytes = net.read(fd, buf)
  }
  const fields = parts.join('').split(' ')
  const comm = fields[1]
  const state = fields[2]
  const [
    ppid,
    pgrp,
    session,
    ttyNr,
    tpgid,
    flags,
    minflt,
    cminflt,
    majflt,
    cmajflt,
    utime,
    stime,
    cutime,
    cstime,
    priority,
    nice,
    numThreads,
    itrealvalue,
    starttime,
    vsize,
    rssPages,
    rsslim,
    startcode,
    endcode,
    startstack,
    kstkesp,
    kstkeip,
    signal,
    blocked,
    sigignore,
    sigcatch,
    wchan,
    nswap,
    cnswap,
    exitSignal,
    processor,
    rtPriority,
    policy,
    delayacctBlkioTicks,
    guestTime,
    cguestTime,
    startData,
    endData,
    startBrk,
    argStart,
    argEnd,
    envStart,
    envEnd,
    exitCode
  ] = fields.slice(3).map(v => Number(v))
  net.close(fd)
  return {
    pid,
    comm,
    state,
    ppid,
    pgrp,
    session,
    ttyNr,
    tpgid,
    flags,
    minflt,
    cminflt,
    majflt,
    cmajflt,
    utime,
    stime,
    cutime,
    cstime,
    priority,
    nice,
    numThreads,
    itrealvalue,
    starttime,
    vsize,
    rssPages,
    rsslim,
    startcode,
    endcode,
    startstack,
    kstkesp,
    kstkeip,
    signal,
    blocked,
    sigignore,
    sigcatch,
    wchan,
    nswap,
    cnswap,
    exitSignal,
    processor,
    rtPriority,
    policy,
    delayacctBlkioTicks,
    guestTime,
    cguestTime,
    startData,
    endData,
    startBrk,
    argStart,
    argEnd,
    envStart,
    envEnd,
    exitCode
  }
}

module.exports = { readStat }
