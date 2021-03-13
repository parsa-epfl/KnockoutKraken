#include "ipc.h"

#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>

int initIPC(ssize_t *socket_fd, const char* filename) {
  // create socket.
  struct sockaddr_un socket_fd_addr;
  socket_fd_addr.sun_family = AF_UNIX;
  strcpy(socket_fd_addr.sun_path, filename);

  *socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if(*socket_fd < 0){
    perror("Socket created error.\n");
    goto failed;
  }

  if(connect(*socket_fd, (struct sockaddr *) &socket_fd_addr, sizeof(socket_fd_addr)) < 0){
    perror("Socket connect error.\n");
    goto failed;
  }
  return 0;

failed:
  return -1;
}

int sendIPC(uint8_t *buffer, ssize_t port, size_t tot_bytes) {
  ssize_t missing_bytes = tot_bytes;
  ssize_t curr_byte = 0;
  ssize_t sent_number = 0;
  while(missing_bytes > 0) {
    sent_number = send(port, &buffer[curr_byte], missing_bytes, MSG_NOSIGNAL);
    if(sent_number > 0) {
      missing_bytes -= sent_number;
      curr_byte += sent_number;
      //printf("Sent[%li]:curr[%li]:missing[%li]\n", sent_number, curr_byte, missing_bytes);
    }
  }
  return 0;
}

int recvIPC(uint8_t *buffer, ssize_t port, size_t expected_bytes) {
  ssize_t missing_bytes = expected_bytes;
  ssize_t curr_byte = 0;
  ssize_t recv_number = 0;
  while(missing_bytes > 0) {
    recv_number = recv(port, &buffer[curr_byte], missing_bytes, MSG_NOSIGNAL);
    if(recv_number > 0) {
      missing_bytes -= recv_number;
      curr_byte += recv_number;
      //printf("Recv[%li]:curr[%li]:missing[%li]\n", recv_number, curr_byte, missing_bytes);
    }
  }
  return 0;
}