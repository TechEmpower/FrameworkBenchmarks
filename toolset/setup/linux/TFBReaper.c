#define _DEFAULT_SOURCE

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include <string.h>

typedef struct Node Node;

/**
 * Simple linked-list struct.
 */
struct Node
{
  char *str; 
  Node *next; 
};

/**
 * References to the head and tail of the linked-list.
 */
Node *head = NULL;
Node *tail = NULL;

/**
 * Reap will recursively find all processes with this process
 * as an ancestor, and kill them.
 */
void reap(int signum)
{
  int pid = getpid();

  FILE *fp;
  char buf[256];

  char command[256];
  sprintf(command, "findChilds() { for child in $(ps --ppid $1 ho pid); do echo $child; findChilds $child; done } && findChilds %d", pid);

  int count;

  do
  {
    count = 0;
    char *pids[256];
    fp = popen(command, "r");
    while(fgets(buf, sizeof(buf), fp) != 0)
    {
      Node *newNode = malloc(sizeof(Node));
      newNode->str = malloc(strlen(buf)+1);
      strcpy(newNode->str, buf);
      newNode->next = NULL;

      if(tail == NULL)
      {
        tail = newNode;
        head = newNode;
      }
      else
      {
        if(head->next == NULL)
        {
          head->next = newNode;
        }
        tail->next = newNode;
        tail = newNode;
      }
      count ++;
    }

    Node *curr = head;
    while(curr != NULL)
    {
      kill(atoi(curr->str), SIGKILL);
      waitpid(atoi(curr->str), NULL, 0);
      curr = curr->next;
    }
  }
  // This may seem magical, but that command from above always results in two
  // additionally PIDs: one for `ps` and one for `sh`. Therefore, all of the
  // lineage of this TFBReaper have been successfully killed once there are
  // only two PIDs counted in the loop.
  // This loop is necessary for edge cases where there is a master->slave 
  // lineage and TFBReaper kills a slave first, which is observed and fixed
  // by the master by spawning a NEW slave in the original's place, and then
  // killing the master (thus orphaning the newly spawned slave, but that PID
  // is not in our master list).
  while(count > 2);

  exit(0);
}

int main(int argc, char *argv[])
{
  // Interrupt SIGTERM and SIGINT and pass to our handler.
  struct sigaction action;
  memset(&action, 0, sizeof(action));
  action.sa_handler = reap;
  sigaction(SIGTERM, &action, NULL);
  sigaction(SIGINT, &action, NULL);

  // Gather the command line arguments for the pass-through.
  int count = argc - 1;
  int *sizes = malloc(sizeof(int) * count);
  int total_size = 0;
  for( int i = 1; i < argc; i++ ) {
    sizes[i - 1] = strlen(argv[i]);
    total_size += sizes[i - 1];
  }
  char *result = malloc(sizeof(char) * total_size + count);
  char *ptr = result;
  for( int i = 1; i < argc; i++ ) {
    memcpy(ptr, argv[i], sizes[i - 1]);
    ptr[sizes[i - 1]] = ' ';
    ptr += sizes[i - 1] + 1;
  }
  *ptr = '\0';
  free(sizes);

  // Here is the magic. This sets any child processes to
  // use THIS process as a 'subreaper'. What that means is
  // even if the process uses the fork-exit technicque for
  // running a daemon (which normally orphans the process
  // and causes init(1) to adopt it, which is problematic
  // for TFB because we cannot then generally kill the
  // process since it has lost all context available to us)
  // the child process will have the parent id of THIS
  // process, allowing us to kill all the processes started
  // by the suite in this way generally.
  //
  // See: http://man7.org/linux/man-pages/man2/prctl.2.html
  prctl(PR_SET_CHILD_SUBREAPER,1);

  // This invokes whatever was passed as arguments to TFBReaper
  // on the system. This program is merely a pass-through to
  // a shell with the subreaper stuff enabled.
  int ret = system(result);

  // We need to wait forever; the suite will clean this 
  // process up later.
  if (ret == 0) {
    for(;;) { 
      // Pause to keep us from spiking CPU; whenever a signal
      // occurs (except SIGTERM etc which will kill this process)
      // just iterate and pause again.
      pause(); 
    }
  }

  // If the scripts failed, we should return that code.
  return ret;
}

