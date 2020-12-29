#define _GNU_SOURCE

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sched.h>
#include <sys/wait.h>
#include <sys/eventfd.h>
#include <netinet/in.h>
#include <linux/filter.h>
#include <err.h>

#include <dynamic.h>
#include <reactor.h>
#include <clo.h>


#define JSON_PREAMBLE "HTTP/1.1 200 OK\r\n"\
                      "Server: L\r\n"\
                      "Content-Type: application/json\r\n"

#define TEXT_PREAMBLE "HTTP/1.1 200 OK\r\n"\
                      "Server: L\r\n"\
                      "Content-Type: text/plain\r\n"


// Returns the full header and trailing \r\n
static segment http_date_header()
{
  static __thread char date_header[38] = "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n";
  segment date = http_date(0);
  memcpy(date_header + 6, date.base, date.size);

  return (segment) {date_header, 37};
}

// Returns the full header and trailing \r\n
// Also includes the final \r\n separating the headers from response body
static segment http_content_length_header(uint32_t n)
{
  // Max content length limited by uint32_t which is 4294967296 (4GB) or 10 chars when written out.
  // 16 (header name) + 10 (header value) + 4 (newlines) + 1 (null terminator) = 31
  static __thread char header[32] = "Content-Length: ";
  size_t length = utility_u32_len(n);

  utility_u32_sprint(n, header + length + 16);
  memcpy(header + length + 16, "\r\n\r\n", 4);

  return (segment) {header, length + 16 + 4};
}

static void write_response(stream *stream, segment preamble, segment body)
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

void plaintext(server_context *context, char *response)
{
  static const segment text_preamble = { .base = TEXT_PREAMBLE, .size = sizeof(TEXT_PREAMBLE) - 1 };
  write_response(&context->session->stream, text_preamble, segment_string(response));
}

void json(server_context *context, clo *json_object)
{
  static const segment json_preamble = { .base = JSON_PREAMBLE, .size = sizeof(JSON_PREAMBLE) - 1 };
  static char json_string[4096];

  (void) clo_encode(json_object, json_string, sizeof(json_string));
  write_response(&context->session->stream, json_preamble, segment_string(json_string));
}

void enable_reuseport_cbpf(server *s)
{
  struct sock_filter code[] = {{BPF_LD | BPF_W | BPF_ABS, 0, 0, SKF_AD_OFF + SKF_AD_CPU}, {BPF_RET | BPF_A, 0, 0, 0}};
  struct sock_fprog prog = { .len = sizeof(code)/sizeof(code[0]), .filter = code };
  int e;

  e = setsockopt(s->fd, SOL_SOCKET, SO_ATTACH_REUSEPORT_CBPF, &prog, sizeof(prog));
  if (e == -1)
    err(1, "SO_ATTACH_REUSEPORT_CBPF");
}

int fork_workers()
{
  int e, efd, worker_count = 0;
  pid_t pid;
  eventfd_t eventfd_value;
  cpu_set_t online_cpus, cpu;

  signal(SIGPIPE, SIG_IGN);

  // Get set/count of all online CPUs
  CPU_ZERO(&online_cpus);
  sched_getaffinity(0, sizeof(online_cpus), &online_cpus);
  int num_online_cpus = CPU_COUNT(&online_cpus);

  // Create a mapping between the relative cpu id and absolute cpu id for cases where the cpu ids are not contiguous
  // E.g if only cpus 0, 1, 8, and 9 are visible to the app because taskset was used or because some cpus are offline
  // then the mapping is 0 -> 0, 1 -> 1, 2 -> 8, 3 -> 9
  int rel_to_abs_cpu[num_online_cpus];
  int rel_cpu_index = 0;

  for (int abs_cpu_index = 0; abs_cpu_index < CPU_SETSIZE; abs_cpu_index++) {
    if (CPU_ISSET(abs_cpu_index, &online_cpus)){
      rel_to_abs_cpu[rel_cpu_index] = abs_cpu_index;
      rel_cpu_index++;

      if (rel_cpu_index == num_online_cpus)
        break;
    }
  }

  // fork a new child/worker process for each available cpu
  for (int i = 0; i < num_online_cpus; i++)
  {
    // Create an eventfd to communicate with the forked child process on each iteration
    // This ensures that the order of forking is deterministic which is important when using SO_ATTACH_REUSEPORT_CBPF
    efd = eventfd(0, EFD_SEMAPHORE);
    if (efd == -1)
      err(1, "eventfd");

    pid = fork();
    if (pid == -1)
      err(1, "fork");

    // Parent process. Block the for loop until the child has set cpu affinity AND started listening on its socket
    if (pid > 0)
    {
      // Block waiting for the child process to update the eventfd semaphore as a signal to proceed
      eventfd_read(efd, &eventfd_value);
      close(efd);

      worker_count++;
      (void) fprintf(stderr, "Worker running on CPU %d\n", i);
      continue;
    }

    // Child process. Set cpu affinity and return eventfd
    if (pid == 0)
    {
      CPU_ZERO(&cpu);
      CPU_SET(rel_to_abs_cpu[i], &cpu);
      e = sched_setaffinity(0, sizeof cpu, &cpu);
      if (e == -1)
        err(1, "sched_setaffinity");

      // Break out of the for loop and continue running main. The child will signal the parent once the socket is open
      return efd;
    }
  }

  (void) fprintf(stderr, "libreactor running with %d worker processes\n", worker_count);

  wait(NULL); // wait for children to exit
  (void) fprintf(stderr, "A worker process has exited unexpectedly. Shutting down.\n");
  exit(EXIT_FAILURE);
}