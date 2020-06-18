#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sched.h>
#include <sys/wait.h>
#include <err.h>


void setup()
{
  int e;
  pid_t pid;
  cpu_set_t available_cpus, cpu;

  signal(SIGPIPE, SIG_IGN);
  CPU_ZERO(&available_cpus);
  sched_getaffinity(0, sizeof(available_cpus), &available_cpus); // Get set of all available CPUs

  for (int i = 0; i < CPU_SETSIZE; i++)
  {
    if (CPU_ISSET(i, &available_cpus))
    {
      pid = fork();
      if (pid == -1)
        err(1, "fork");

      if (pid == 0)
      {
       CPU_ZERO(&cpu);
       CPU_SET(i, &cpu);
        e = sched_setaffinity(0, sizeof cpu, &cpu);
        if (e == -1)
          err(1, "sched_setaffinity");

        return;
      }
    }
  }
  wait(NULL);
}