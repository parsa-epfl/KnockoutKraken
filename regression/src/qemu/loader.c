#define _GNU_SOURCE
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include "devteroflex.h"

static void enterDevteroFlex(){
  puts("I enter the system!");
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
}

static void leaveDevteroFlex(){
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  puts("I left the system!");
}

int main(int argc, char * const argv[], char * const envp[]){
  if(argc < 2){
    printf("Usage: %s program [opts] \n", argv[0]);
    exit(0);
  }

  // prepare argv for the next program
  char **new_argv = calloc(argc, sizeof(char *));
  for(int i = 0; i < argc - 1; ++i){
    new_argv[i] = argv[i+1];
  }

  // start forking.
  pid_t pid = fork();
  int exitStatus = -1;
  int exited = -1;
  int signaled = -1;
  int term = -1;

  if(pid == 0) {
    // child progress.
    enterDevteroFlex();
    execvpe(new_argv[0], new_argv, envp);
  } else {
    // the parent, wait for the subprocess to run, and then stop if it's necessary.
    int stat = 0;
    do {
      waitpid(pid, &stat, 0);
      exited = WIFEXITED(stat);
      signaled = WIFSIGNALED(stat);
      term = WTERMSIG(stat);
    } while(!exited && !signaled);
    leaveDevteroFlex();
    exitStatus = WEXITSTATUS(stat);
    if(!exited) {
      printf("Child did not terminate with exited[%i]:signaled[%i]:term[%i]:status[%i]\n", exited, signaled, term, exitStatus);
    }
    free(new_argv);
  }

  if(exitStatus == 0) {
    printf("Success %s\n", argv[1]);
    return 0;
  } else {
    printf("Failed %i\n", exitStatus);
    printf("Failed exited[%i]:signaled[%i]:term[%i]:status[%i]\n", exited, signaled, term, exitStatus);
    return exitStatus;
  }
}
