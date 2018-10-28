#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sched.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <err.h>

#include <dynamic.h>

struct cpu
{
  int       id;
  pid_t     pid;
  cpu_set_t set;
};

int realtime(void)
{
  struct sched_param param;
  struct rlimit rlim;
  int e;

  e = sched_getparam(0, &param);
  if (e == -1)
    return -1;

  param.sched_priority = sched_get_priority_max(SCHED_FIFO);
  rlim = (struct rlimit) {.rlim_cur = param.sched_priority, .rlim_max = param.sched_priority};
  e = prlimit(0, RLIMIT_RTPRIO, &rlim, NULL);
  if (e == -1)
    return -1;

  e = sched_setscheduler(0, SCHED_FIFO, &param);
  if (e == -1)
    return -1;

  return 0;
}

static void cpu_list(vector *list, int sib)
{
  struct cpu cpu;
  char path[1024], buf[1024];
  size_t i, id;
  ssize_t n;
  int fd;

  vector_construct(list, sizeof cpu);
  for (i = 0;; i ++)
    {
      snprintf(path, sizeof path, "/sys/devices/system/cpu/cpu%lu/topology/thread_siblings_list", i);
      fd = open(path, O_RDONLY);
      if (fd == -1)
        break;
      n = read(fd, buf, sizeof buf - 1);
      if (n == -1)
        err(1, "read");
      buf[n] = 0;
      close(fd);

      id = strtoul(buf, NULL, 0);
      if (!sib && id != i)
        continue;

      cpu = (struct cpu) {0};
      cpu.id = id;
      CPU_ZERO(&cpu.set);
      CPU_SET(id, &cpu.set);
      vector_push_back(list, &cpu);
    }
}

void setup(size_t skip, int sib)
{
  vector list;
  struct cpu *cpu;
  size_t i;
  int e;
  pid_t cpid;

  signal(SIGPIPE, SIG_IGN);
  cpu_list(&list, sib);
  for (i = 0; i < vector_size(&list); i ++)
    {
      if (i % skip != 0)
        continue;
      cpu = vector_at(&list, i);
      cpid = fork();
      if (cpid == -1)
        err(1, "fork");
      if (cpid == 0)
        {
          e = sched_setaffinity(0, sizeof cpu->set, &cpu->set);
          if (e == -1)
            err(1, "sched_setaffinity");
          return;
        }
      cpu->pid = cpid;
    }
  wait(NULL);
  vector_destruct(&list, NULL);
}
