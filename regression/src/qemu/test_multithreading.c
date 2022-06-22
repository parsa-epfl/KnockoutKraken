#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

#include "devteroflex.h"

static int counter = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

const int THREAD_COUNT = 1024;

void *inc(void *x){
  usleep(1e4);
  pthread_mutex_lock(&mutex);
  counter++;
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(){
  pthread_t threads[THREAD_COUNT];

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);

  for(int i = 0; i < THREAD_COUNT; ++i){
    pthread_create(&threads[i], NULL, inc, NULL);
  }

  for(int i = 0; i < THREAD_COUNT; ++i){
    pthread_join(threads[i], NULL);
  }

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);


  printf("The counter is %d \n", counter);
  assert(counter == THREAD_COUNT);

  return 0;
}
