#include <stdio.h>
#include <stdlib.h>
#include <thread>
#include <time.h>

int64_t fibonacciNumber(uint32_t index) {
  if (index == 0)
    return 0;
  if (index == 1)
    return 1;
  return fibonacciNumber(index - 1) + fibonacciNumber(index - 2);
}

int main() {
  srand(time(nullptr));
  constexpr int THREAD_NUMBER = 8;
  // start recording trace
  asm volatile("hint 91");
  asm volatile("hint 90");
  asm volatile("hint 90");
  asm volatile("hint 92");
  std::thread *threads[THREAD_NUMBER];
  for (int i = 0; i < THREAD_NUMBER; ++i) {
    threads[i] = new std::thread([i]() {
      asm volatile("hint 90");
      asm volatile("hint 92");
      printf("ThreadID: %d, CalcNumber: %lx \n", i, fibonacciNumber(25));
    });
  }
  for (int i = 0; i < THREAD_NUMBER; ++i) {
    threads[i]->join();
  }
  for (int i = 0; i < THREAD_NUMBER; ++i) {
    delete threads[i];
  }
  // stop recording trace
  asm volatile("hint 91");
  asm volatile("hint 91");
  return 0;
}
