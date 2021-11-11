const { http } = just.library('http.so', 'http')
const { parseRequests, getRequests, getUrl } = http

const free = []

function createParser (buffer) {
  if (free.length) {
    const parser = free.shift()
    parser.buffer.offset = 0
    return parser
  }
  const answer = [0]
  const parser = { buffer }
  function parse (bytes, off = 0) {
    const count = parseRequests(buffer, buffer.offset + bytes, off, answer)
    if (count > 0) {
      parser.onRequests(count)
    }
    if (answer[0] > 0) {
      const start = buffer.offset + bytes - answer[0]
      const len = answer[0]
      if (start > buffer.offset) {
        buffer.copyFrom(buffer, 0, len, start)
      }
      buffer.offset = len
      return
    }
    buffer.offset = 0
  }
  buffer.offset = 0
  parser.parse = parse
  parser.get = count => getRequests(count)
  parser.url = index => getUrl(index)
  parser.free = () => free.push(parser)
  return parser
}

module.exports = { createParser }
