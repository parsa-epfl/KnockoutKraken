#include <mutex>
#include <atomic>
#include <condition_variable>
#include <thread>

static std::mutex thread_cnt_lock;
static int thread_cnt;

static std::mutex sub_to_top_m;
static std::condition_variable sub_to_top;

static std::mutex top_to_sub_m;
static std::condition_variable top_to_sub;


class SyncClockRoutine {
public:
  SyncClockRoutine(){
  }

  void waitTick(){
    // try to notify the host
    {
      std::lock_guard<std::mutex> top_lock(thread_cnt_lock);
      // add
      ++thread_cnt;
      // notify someone
      if(thread_cnt == 6) sub_to_top.notify_all();
    }
    std::unique_lock<std::mutex> lock(top_to_sub_m);
    // wait for the signal from top
    top_to_sub.wait(lock);
  }

  void operator() (){
    
    for(int i = 0; i < 10; ++i){
      printf("%d \n", i);
      waitTick();
    }
    
  }
};



int main(){
  constexpr int THREAD_NUMBER = 6;
  // set the number to zero.
  std::unique_lock<std::mutex> top_lock(thread_cnt_lock);
  thread_cnt = 0;  

  std::thread t[THREAD_NUMBER];
  for(int i = 0; i < THREAD_NUMBER; ++i){
    t[i] = std::thread(SyncClockRoutine());
  }

  for(int i = 0; i < 10; ++i){
    // wait for the signal to be true.
    sub_to_top.wait(top_lock, [&](){
      return thread_cnt == THREAD_NUMBER;
    });

    thread_cnt = 0;
    top_to_sub.notify_all();
  }

  for(auto &sub_t : t){
    sub_t.join();
  }

  return 0;
}

