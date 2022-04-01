#define _GNU_SOURCE

#include "ipc.h"

#include <stdlib.h>
#include <bits/types/error_t.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <asm-generic/errno-base.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h> 
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>

int initIPC_client(ssize_t *socket_fd, const char* filename) {
  // create socket.
  struct sockaddr_un socket_fd_addr;
  socket_fd_addr.sun_family = AF_UNIX;
  strcpy(socket_fd_addr.sun_path, filename);

  *socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if(*socket_fd < 0){
    perror("Socket created error.\n");
    goto failed;
  }

  while(connect(*socket_fd, (struct sockaddr *) &socket_fd_addr, sizeof(socket_fd_addr)) < 0) {
    sleep(3);
  }
  return 0;

failed:
  return -1;
}

int initIPC_server(ssize_t *client_fd, const char* name) {
  // create socket
  ssize_t socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if(socket_fd < 0){
    perror("Socket created error.");
    goto failed;
  }

  // bind
  struct sockaddr_un my_addr, peer_addr;
  socklen_t addr_len;
  my_addr.sun_family = AF_UNIX;
  strcpy(my_addr.sun_path, name);

  if(bind(socket_fd, (struct sockaddr *) &my_addr, sizeof(struct sockaddr_un)) < 0){
    // bind error.
    perror("Socket bind error.");
    goto failed;
  }

  // listen
  if(listen(socket_fd, 1) < 0){
    perror("Socket listen error.");
    goto failed;
  }

  addr_len = sizeof(peer_addr);
  *client_fd = accept4(socket_fd, (struct sockaddr *) &peer_addr, &addr_len, SOCK_NONBLOCK);

  if(client_fd < 0){
    perror("Socket accept error.");
    goto failed;
  }

  printf("Socket %s correctly established.\n", name);

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
    //printf("Sent[%li]:curr[%li]:missing[%li]\n", sent_number, curr_byte, missing_bytes);
    if(sent_number > 0) {
      missing_bytes -= sent_number;
      curr_byte += sent_number;
   }
  }
  return 0;
}

static bool recvIPC_internal(uint8_t *buffer, ssize_t port, size_t expected_bytes, bool force_recv) {
  ssize_t missing_bytes = expected_bytes;
  ssize_t curr_byte = 0;
  ssize_t recv_number = 0;
  int block = force_recv ? MSG_NOSIGNAL : 0;
  do {
    recv_number = recv(port, &buffer[curr_byte], missing_bytes, block);
    if(recv_number > 0) {
      //printf("Received %lu bytes from %lu\n", recv_number, expected_bytes);
      missing_bytes -= recv_number;
      curr_byte += recv_number;
    } else if (recv_number == -1) {
      switch(errno) {
        case EAGAIN:
          if (curr_byte == 0) {
            goto empty_message;
          };
          break;
        default:
          printf("ERROR:SOCKET:recv return error not handled? errno[%s]\n", strerror(errno));
          goto error;
      }
    } else if (recv_number == 0) {
      perror("ERROR:SOCKET: Broken pipe for recv AXI.\n");
      goto error;
    }
  } while(missing_bytes > 0);

  return 0;

empty_message:
  return EMPTY_MESSAGE;

error:
  printf("RECV returns error");
  exit(1);
}

int recvIPC(uint8_t *buffer, ssize_t port, size_t expected_bytes) {
  return recvIPC_internal(buffer, port, expected_bytes, true);
}

int recvIPC_nonblocky(uint8_t *buffer, ssize_t port, size_t expected_bytes) {
  return recvIPC_internal(buffer, port, expected_bytes, false);
}

