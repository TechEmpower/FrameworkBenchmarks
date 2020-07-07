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

static void cpu_list(vector *list, int use_all_cores)
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

      if (use_all_cores){
        id = i; /* add every cpu (whether physical or logical) to the list */
      }
      else if (id != i){
        continue; /* ignore sibling CPUs */
      }

      cpu = (struct cpu) {0};
      cpu.id = id;
      CPU_ZERO(&cpu.set);
      CPU_SET(id, &cpu.set);
      vector_push_back(list, &cpu);
    }
}

void setup()
{
  vector list;
  struct cpu *cpu;
  size_t i;
  int e;
  pid_t cpid;

  signal(SIGPIPE, SIG_IGN);
  cpu_list(&list, 1);

  for (i = 0; i < vector_size(&list); i++)
    {
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
