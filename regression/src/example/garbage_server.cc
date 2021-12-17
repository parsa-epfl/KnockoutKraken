#include <stdio.h>
#include <unistd.h> 
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>

const char SOCKET_NAME[] = "garbage.server";

int main(int argc, char **argv){
  int socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);

  if(socket_fd < 0){
    perror("Socket created error.");
    return 1;
  }

  sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  strcpy(addr.sun_path, SOCKET_NAME);

  // bind
  if(bind(socket_fd, reinterpret_cast<sockaddr *>(&addr), sizeof(addr)) < 0){
    perror("Socket bind error.");
    return -1;
  }

  // listen
  if(listen(socket_fd, 5) < 0){
    perror("Socket listen error.");
    return -1;
  }

  for(;;){
    int data_socket = accept(socket_fd, nullptr, nullptr);
    if(data_socket < 0){
      perror("Socket accept errpr.");
      return -1;
    }
    puts("Connection established.");
    for(;;){
      // read data
      char buffer[128];
      memset(buffer, 0, 128);
      ssize_t read_number = read(data_socket, buffer, 128);
      if(read_number < 0){
        perror("Socket read error.");
        return -1;
      }
      // print the data
      printf("Received messages: %s. \n", buffer);
      // reply data
      for(ssize_t i = 0; i < read_number; ++i){
        if(buffer[i] >= 'a' && buffer[i] <= 'z'){
          buffer[i] = buffer[i] - 'a' + 'A';
        }
      }
      if(write(data_socket, buffer, read_number) < 0){
        perror("Socket write error.");
        return -1;
      }
    }
  }

  return 0;
}