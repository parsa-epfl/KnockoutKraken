#include "ipc.hh"

#include <asm-generic/errno-base.h>
#include <mutex>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h> 
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>

IPCServer::IPCServer(const std::string &name) : name(name) {
  // create socket
  error = false;
  socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if(socket_fd < 0){
    perror("Socket created error.");
    error = true;
    return;
  }

  // bind
  sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  strcpy(addr.sun_path, name.data());

  if(bind(socket_fd, reinterpret_cast<sockaddr *>(&addr), sizeof(addr)) < 0){
    // bind error.
    perror("Socket bind error.");
    error = true;
    return;
  }

  // listen
  if(listen(socket_fd, 1) < 0){
    perror("Socket listen error.");
    error = true;
    return;
  }

  client_fd = -1;
}

IPCServer::~IPCServer(){
  if(socket_fd > 0){
    close(socket_fd);
  }
  if(client_fd > 0){
    close(client_fd);
  }
}

bool IPCServer::waitForClient(){
  if(error) return false;

  client_fd = accept4(socket_fd, nullptr, nullptr, SOCK_NONBLOCK);
  if(client_fd < 0){
    perror("Socket accept error.");
    error = true;
    return false;
  }

  printf("Socket %s correctly established.\n", name.data());

  return true;
}

bool IPCServer::getRequestAXIL(MemoryRequestAXIL *msg){
  ssize_t missing_bytes = sizeof(MemoryRequestAXIL);
  ssize_t curr_byte = 0;
  ssize_t read_number = 0;
  while(missing_bytes > 0) {
    if (error) {
      perror("ERROR:SOCKET: IPC Error was set somewhere else, but didn't quit for some reason\n");
      return false;
    }
    read_number = recv(client_fd, &msg[curr_byte], missing_bytes, 0);
    if (read_number == -1) {
      switch(errno) {
        case EAGAIN:
          if (curr_byte == 0) return false;
          break;
        default:
          perror("ERROR:SOCKET:AXIL recv return error not handled.\n");
          error = true;
          return false;
      }
    }
    if (read_number == 0) {
      // No pending requests
      error = true;
      perror("ERROR:SOCKET: Broken pipe for recv AXIL.\n");
      return false;
    }
    if(read_number > 0) {
      missing_bytes -= read_number;
      curr_byte += read_number;
      //printf("Recv[%li]:curr[%li]:miss[%li]\n", read_number, curr_byte, missing_bytes);
    }
  }
  return true;
}


bool IPCServer::getRequestAXI(MemoryRequest *msg){
  ssize_t missing_bytes = sizeof(MemoryRequest);
  ssize_t curr_byte = 0;
  ssize_t read_number = 0;
  while(missing_bytes > 0) {
    if (error) {
      perror("ERROR:SOCKET: IPC Error was set somewhere else, but didn't quit for some reason\n");
      return false;
    }
    read_number = recv(client_fd, &msg[curr_byte], missing_bytes, 0);
    if (read_number == -1) {
      switch(errno) {
        case EAGAIN:
          if (curr_byte == 0) return false;
          break;
        default:
          perror("ERROR:SOCKET:AXI recv return error not handled?\n");
          error = true;
          return false;
      }
    }
    if (read_number == 0) {
      error = true;
      perror("ERROR:SOCKET: Broken pipe for recv AXI.\n");
      return false;
    }

    if(read_number > 0) {
      missing_bytes -= read_number;
      curr_byte += read_number;
    }
  }

  return true;
}

bool IPCServer::reply(const uint32_t *data, size_t word_count){
  uint8_t *buff = (uint8_t *) data;
  ssize_t missing_bytes = word_count*4;
  ssize_t curr_byte = 0;
  ssize_t sent_number = 0;
  while(missing_bytes > 0) {
    if (error) {
      perror("ERROR:SOCKET: IPC Error was set somewhere else, but didn't quit for some reason\n");
      return false;
    }
    sent_number = send(client_fd, &buff[curr_byte], missing_bytes, 0);
    if (sent_number == -1) {
      switch(errno) {
        case EPIPE:
          error = true;
          perror("ERROR:SOCKET: Broken pipe for send.\n");
          return false; 
        case EAGAIN:
          return false;
        default:
          perror("ERROR:SOCKET: Error code for send() not handled.\n");
          return false;
      }
    }
    if(sent_number > 0) {
      missing_bytes -= sent_number;
      curr_byte += sent_number;
    }
  }

  return true;
}

bool IPCServer::hasPendingError(void) {
  return error;
}
