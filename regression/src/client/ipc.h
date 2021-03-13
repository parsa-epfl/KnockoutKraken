#include <stdint.h>
#include <unistd.h>

typedef struct MemoryRequest {
  size_t addr;
  size_t byte_size;
  uint32_t is_write;
  uint32_t w_data[1024]; // 4k at most
} MemoryRequest;

typedef struct MemoryRequestAXIL {
  size_t addr;
  size_t byte_size;
  uint32_t is_write;
  uint32_t w_data;
} MemoryRequestAXIL;

int initIPC(ssize_t *socket_fd, const char* filename);
int sendIPC(uint8_t *buffer, ssize_t port, size_t tot_bytes);
int recvIPC(uint8_t *buffer, ssize_t port, size_t expected_bytes);
 