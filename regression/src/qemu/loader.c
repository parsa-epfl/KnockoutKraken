#define _GNU_SOURCE
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>

#define STR(x)  #x                                                                                                                               
#define XSTR(s) STR(s)                                                                                                                           
#define magic_inst(val) __asm__ __volatile__ ( "hint " XSTR(val) " \n\t"  )                                                                      
                                                                                                                                                 
#define DO_QFLEX_OP(op) magic_inst(QFLEX_OP); magic_inst(op)                                                                                     
                                                                                                                                                 
#define DEVTEROFLEX_OP    (94)

#define DEVTEROFLEX_FLOW_START (90)
#define DEVTEROFLEX_FLOW_STOP  (91)
#define DO_DEVTEROFLEX_OP(op) magic_inst(DEVTEROFLEX_OP); magic_inst(op) 


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

  if(pid == 0) {
    // child progress.
    enterDevteroFlex();
    execvpe(new_argv[0], new_argv, envp);
  } else {
    // the parent, wait for the subprocess to run, and then stop if it's necessary.
    int stat = 0;
    do {
      waitpid(pid, &stat, 0);
    } while(!WIFEXITED(stat) && !WIFSIGNALED(stat));
    leaveDevteroFlex();
    free(new_argv);
  }
  return 0;
}
