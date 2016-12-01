#include <stdlib.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <asm/unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
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
  free(result);

  // This tells the application to wait until all the child 
  // processes have completed.
  int status;
  wait(&status);

  // We still need to wait forever in order to properly clean
  // existing processes started by us.
  for(;;){}

  return ret;
}

