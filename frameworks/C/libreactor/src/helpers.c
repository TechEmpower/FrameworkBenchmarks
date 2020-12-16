#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <string.h>
#include <err.h>

#include <dynamic.h>
#include <reactor.h>
#include <clo.h>

// Returns the full header and trailing \r\n
segment http_date_header()
{
  static __thread char date_header[38] = "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n";
  segment date = http_date(0);
  memcpy(date_header + 6, date.base, date.size);

  return (segment) {date_header, 37};
}

// Returns the full header and trailing \r\n
// Also includes the final \r\n separating the headers from response body
segment http_content_length_header(uint32_t n)
{
  // Max content length limited by uint32_t which is 4294967296 (4GB) or 10 chars when written out.
  // 16 (header name) + 10 (header value) + 4 (newlines) + 1 (null terminator) = 31
  static __thread char header[32] = "Content-Length: ";
  size_t length = utility_u32_len(n);

  utility_u32_sprint(n, header + length + 16);
  memcpy(header + length + 16, "\r\n\r\n", 4);

  return (segment) {header, length + 16 + 4};
}

void write_response(stream *stream, segment preamble, segment body)
{
  segment date_header = http_date_header(0); // includes header name, value, and \r\n
  segment content_length_header = http_content_length_header(body.size); // includes header name, value, and \r\n\r\n
  size_t response_size = preamble.size + date_header.size + content_length_header.size + body.size;

  // Reserves additional space in the stream's output buffer (if needed) and updates the buffer size
  // stream_allocate returns a segment which we convert to a char * pointer
   char *output_buffer_ptr = (char *) (stream_allocate(stream, response_size)).base;

  // memcpy the response directly to the output stream buffer
  memcpy(output_buffer_ptr, preamble.base, preamble.size);
  memcpy(output_buffer_ptr + preamble.size, date_header.base, date_header.size);
  memcpy(output_buffer_ptr + preamble.size + date_header.size, content_length_header.base, content_length_header.size);
  memcpy(output_buffer_ptr + preamble.size + date_header.size + content_length_header.size, body.base, body.size);
}