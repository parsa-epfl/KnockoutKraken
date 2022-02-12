#pragma once

#include <string>
#include <mutex>
#include <queue>
#include <thread>

/**
 * @file define a wrapper based on Unix domain socket that is used for IPC.
 */

struct MemoryRequest {
  uint64_t addr;
  size_t byte_size;
  uint32_t is_write;
  uint32_t w_data[1024]; // 4k at most // TODO: optimize the
};

struct MemoryRequestAXIL {
  uint64_t addr;
  size_t byte_size;
  uint32_t is_write;
  uint32_t w_data;
};


class IPCServer final {
public:
  IPCServer(const std::string &name);
  ~IPCServer();

  /**
   * Wait for a client to connect.
   * 
   * @return true if it's successful connection.
   * 
   * @note this will block current thread until a connection arrives.
   */ 
  bool waitForClient();

  /**
   * Read request from the Unix domain socket.
   * 
   * @param msg the expected read result
   * @return true if the result is valid.
   * 
   * @note this function will not block the thread.
   */ 
  bool getRequestAXIL(MemoryRequestAXIL *msg);

  /**
   * Read request from the Unix domain socket.
   * 
   * @param msg the expected read result
   * @return true if the result is valid.
   * 
   * @note this function will not block the thread.
   */ 
  bool getRequestAXI(MemoryRequest *msg);


  /**
   * Reply to the request.
   * 
   * @param data the data to reply
   * @param word_count the number of the word
   * 
   * @return true if the operation is successful.
   * 
   */ 
  bool reply(const uint32_t *data, size_t word_count);

  /**
   * Has a pending error to be handled
   * @return `error` field
   */
  bool hasPendingError(void);

private:
  // name of the thread
  std::string name;
  // socket related.
  int socket_fd;
  int client_fd;
  bool error;
};

