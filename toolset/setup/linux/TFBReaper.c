#define _DEFAULT_SOURCE

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
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

// Stores the trimmed input string into the given output buffer, which must be
// large enough to store the result.  If it is too small, the output is
// truncated.
size_t trimwhitespace(char *out, size_t len, const char *str)
{
  if(len == 0)
    return 0;

  const char *end;
  size_t out_size;

  // Trim leading space
  while(isspace((unsigned char)*str)) str++;

  if(*str == 0)  // All spaces?
  {
    *out = 0;
    return 1;
  }

  // Trim trailing space
  end = str + strlen(str) - 1;
  while(end > str && isspace((unsigned char)*end)) end--;
  end++;

  // Set output size to minimum of trimmed string length and buffer size minus 1
  out_size = (end - str) < len-1 ? (end - str) : len-1;

  // Copy trimmed string and add null terminator
  memcpy(out, str, out_size);
  out[out_size] = 0;

  return out_size;
}

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
  sprintf(command, "findChilds() { for child in $(ps --ppid $1 ho pid); do ps --no-headers -o pid,ppid,command $child; findChilds $child; done } && findChilds %d", pid);

  int count;

  do
  {
    count = 0;
    fp = popen(command, "r");
    while(fgets(buf, sizeof(buf), fp) != 0)
    {
      char trimmed[256];
      trimwhitespace(trimmed, 256, buf);
      char *childPid = strtok(trimmed," ");
      printf("%s\n", buf);
      Node *newNode = malloc(sizeof(Node));
      newNode->str = malloc(strlen(childPid)+1);
      strcpy(newNode->str, childPid);
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

    printf("Number of pids: %i\n", count);
  }
  // This may seem magical, but that command from above always results in one
  // additional PID: one for `sh` executing fildChilds. Therefore, all of the
  // lineage of this TFBReaper have been successfully killed once there is
  // only one PID counted in the loop.
  // This loop is necessary for edge cases where there is a master->slave 
  // lineage and TFBReaper kills a slave first, which is observed and fixed
  // by the master by spawning a NEW slave in the original's place, and then
  // killing the master (thus orphaning the newly spawned slave, but that PID
  // is not in our master list).
  while(count > 1);

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

