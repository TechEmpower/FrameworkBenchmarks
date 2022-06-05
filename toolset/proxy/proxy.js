const { net } = just.library('net')
const { sys } = just.library('sys')
const dns = require('@dns')
const binary = require('@binary')

const { EPOLLIN, EPOLLOUT, EPOLLERR, EPOLLHUP, EPOLLET } = just.loop
const { getIPAddress } = dns
const { 
  AF_INET, 
  SOCK_STREAM, 
  SOMAXCONN, 
  SOL_SOCKET, 
  SO_REUSEADDR, 
  SO_REUSEPORT,
  SO_ERROR,
  O_NONBLOCK,
  EAGAIN
} = net
const { loop } = just.factory
const { SystemError } = just
const { errno, strerror, fcntl, F_GETFL, F_SETFL } = sys
const { AG, AR, AY, AD, AM } = binary.ANSI

const messageTypes = {
  backend: {
    AuthenticationOk: 82,
    ErrorResponse: 69,
    RowDescription: 84,
    CommandComplete: 67,
    ParseComplete: 49,
    BindComplete: 50,
    ReadyForQuery: 90,
    BackendKeyData: 75,
    ParameterStatus: 83,
    ParameterDescription: 116,
    DataRow: 68,
    NoticeResponse: 78
  },
  frontend: {
    Prepare: 80,
    Flush: 72,
    Bind: 66,
    StartupMessage: 0,
    PasswordMessage: 112,
    Exec: 69,
    Describe: 68,
    Prepare: 80,
    Bind: 66,
    Sync: 83,
    Query: 81,
    Terminate: 88
  }
}

const lookup = {
  backend: {},
  frontend: {}
}
for (const key of Object.keys(messageTypes.backend)) {
  lookup.backend[messageTypes.backend[key]] = key
}
for (const key of Object.keys(messageTypes.frontend)) {
  lookup.frontend[messageTypes.frontend[key]] = key
}

class PGParser {
  constructor (buf, direction = 'frontend') {
    this.buf = buf
    this.type = 0
    this.code = ''
    this.name = ''
    this.len = 0
    this.pos = 0
    this.want = 0
    this.u8 = new Uint8Array(buf)
    this.direction = direction
  }

  parse (bytes) {
    const messages = []
    const { u8 } = this
    let off = 0
    while (off < bytes) {
      if (this.pos === 0) {
        this.len = 0
        this.want = 0
        this.type = u8[off]
        if (this.type === 0) {
          this.code = ''
        } else {
          this.code = String.fromCharCode(this.type)
        }
        this.name = lookup[this.direction][this.type]
        this.pos++
      } else if (this.pos === 1) {
        if (this.type === 0) {
          this.len += u8[off] << 16
        } else {
          this.len += u8[off] << 24
        }
        this.pos++
      } else if (this.pos === 2) {
        if (this.type === 0) {
          this.len += u8[off] << 8
        } else {
          this.len += u8[off] << 16
        }
        this.pos++
      } else if (this.pos === 3) {
        if (this.type === 0) {
          this.len += u8[off]
          this.want = this.len - 4
        } else {
          this.len += u8[off] << 8
        }
        this.pos++
      } else if (this.pos === 4) {
        if (this.type === 0) {
          this.want--
        } else {
          this.len += u8[off]
          this.want = this.len - 4
        }
        if (this.want === 0) {
          const { type, code, name, len } = this
          messages.push({ type, code, name, len })
          this.pos = 0
        } else {
          this.pos++
        }
      } else {
        this.want--
        if (this.want === 0) {
          const { type, code, name, len } = this
          messages.push({ type, code, name, len })
          this.pos = 0
        } else {
          this.pos++
        }
      }
      off++
    }
    return messages
  }
}

function nonBlocking (fd) {
  let flags = fcntl(fd, F_GETFL, 0)
  if (flags < 0) return
  fcntl(fd, F_SETFL, flags | O_NONBLOCK)
}

function pad (val) {
  return val.toString().padStart(8, ' ')
}

async function main () {
  just.print(`${AY}***${AD} ${AM}POSTGRES PROTOCOL VERIFICATION${AD} ${AY}***${AD}`)
  const listenFd = net.socket(AF_INET, SOCK_STREAM | O_NONBLOCK, 0)
  net.setsockopt(listenFd, SOL_SOCKET, SO_REUSEADDR, 1)
  net.setsockopt(listenFd, SOL_SOCKET, SO_REUSEPORT, 1)
  if (net.bind(listenFd, '0.0.0.0', 5432) !== 0) throw new SystemError('bind')
  if (net.listen(listenFd, SOMAXCONN) !== 0) throw new SystemError('listen')
  let ip
  while (1) {
    try {
      ip = await getIPAddress('tfb-proxy-database')
      break
    } catch (err) {
      just.sleep(1)
    }
  }
  loop.add(listenFd, (fd, event) => {
    const buf = new ArrayBuffer(65536)
    const parser = new PGParser(buf, 'frontend')
    if (event & EPOLLIN) {
      const front = net.accept(fd)
      if (front === -1) {
        net.close(front)
        return
      }
      nonBlocking(front)
      const back = net.socket(AF_INET, SOCK_STREAM, 0)
      nonBlocking(back)
      if (net.connect(back, ip, 5431) === -1 && errno() !== 115) {
        net.close(back)
        net.close(front)
        return
      }
      let backendConnected = false
      loop.remove(front)

      const stats = {}
      let missingSyncs = 0
      let expectSync = false

      function onClose () {
        if (stats.Sync > 0) {
          let status = 'ok'
          if (!((stats.Bind === stats.Exec) && (stats.Sync >= stats.Exec)) || missingSyncs > 0) {
            status = 'fail'
          }
          just.print(`${AY}Query${AD} ${pad(stats.Query || 0)} ${AY}Bind${AD} ${pad(stats.Bind || 0)} ${AY}Exec${AD} ${pad(stats.Exec || 0)} ${AY}Sync${AD} ${pad(stats.Sync || 0)} ${AY}Miss${AD} ${pad(missingSyncs)} ${status === 'ok' ? AG : AR }${status}${AD}`)
        }
      }
      const frontend = {
        paused: false,
        handler: null,
        pendingBytes: 0,
        off: 0
      }
      const backend = {
        paused: false,
        handler: null,
        pendingBytes: 0,
        off: 0
      }
      loop.add(back, (back, event) => {
        if (event & EPOLLOUT) {
          if (backendConnected) {
            if (backend.paused) {
              //just.print('backend writable again')
              net.write(back, buf, backend.pendingBytes, backend.off)
              loop.add(front, backend.handler, EPOLLIN | EPOLLOUT)
              backend.paused = false
              backend.off = 0
            }
          } else {
            backendConnected = true
            let frontendConnected = false
            loop.add(front, (front, event) => {
              if (event & EPOLLOUT) {
                if (frontendConnected) {
                  if (frontend.paused) {
                    //just.print('frontend writable again')
                    net.write(front, buf, frontend.pendingBytes, frontend.off)
                    loop.add(back, frontend.handler, EPOLLIN | EPOLLOUT)
                    frontend.paused = false
                    frontend.off = 0
                  }
                } else {
                  frontendConnected = true
                }
              }
              if (event & EPOLLIN) {
                const bytes = net.read(front, buf, 0, buf.byteLength)
                if (bytes === 0) {
                  net.close(front)
                  net.close(back)
                  onClose()
                } else if (bytes > 0) {
                  const messages = parser.parse(bytes)
                  for (const message of messages) {
                  if (stats[message.name]) {
                      stats[message.name]++
                    } else {
                      stats[message.name] = 1
                    }
                    if (expectSync && message.name !== 'Sync') {
                      missingSyncs++
                    }
                    if (message.name === 'Exec') {
                      expectSync = true
                    } else {
                      expectSync = false
                    }
                  }
                  const written = net.write(back, buf, bytes)
                  if (written < bytes) {
                    //just.print(`backend short write ${written} of ${bytes} (${errno()}) ${strerror(errno())}`)
                    if (written === -1) {
                      if (errno() === EAGAIN) {
                        backend.handler = loop.handles[front]
                        loop.remove(front)
                        backend.pendingBytes = bytes
                        backend.paused = true
                      }
                    } else {
                      backend.handler = loop.handles[front]
                      loop.remove(front)
                      backend.pendingBytes = bytes - written
                      backend.off = written
                      backend.paused = true
                    }
                  }
                } else {
                  net.close(front)
                  net.close(back)
                  onClose()
                }
              }
              if (event & EPOLLERR) {
                const err = net.getsockopt(front, SOL_SOCKET, SO_ERROR)
                net.close(front)
                net.close(back)
                onClose()
              }
              if (event & EPOLLHUP) {
                net.close(front)
                net.close(back)
                onClose()
              }
            }, EPOLLIN | EPOLLOUT)
          }
        }
        if (event & EPOLLIN) {
          const bytes = net.read(back, buf, 0, buf.byteLength)
          if (bytes === 0) {
            net.close(back)
            net.close(front)
          } else if (bytes > 0) {
            const written = net.write(front, buf, bytes)
            if (written < bytes) {
              //just.print(`frontend short write ${written} of ${bytes} (${errno()}) ${strerror(errno())}`)
              if (written === -1) {
                if (errno() === EAGAIN) {
                  frontend.handler = loop.handles[back]
                  loop.remove(back)
                  frontend.pendingBytes = bytes
                  frontend.paused = true
                }
              } else {
                frontend.handler = loop.handles[back]
                loop.remove(back)
                frontend.pendingBytes = bytes - written
                frontend.off = written
                frontend.paused = true
              }
            }
          } else {
            net.close(front)
            net.close(back)
          }
        }
        if (event & EPOLLERR) {
          const err = net.getsockopt(front, SOL_SOCKET, SO_ERROR)
          net.close(front)
          net.close(back)
          return
        }
        if (event & EPOLLHUP) {
          net.close(front)
          net.close(back)
        }
      }, EPOLLIN | EPOLLOUT)
    }
  }, EPOLLIN | EPOLLOUT)
}

main().catch(err => just.error(err.stack))
