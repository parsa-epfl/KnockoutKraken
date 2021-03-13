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
    return -1;
  }

  sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  strcpy(addr.sun_path, SOCKET_NAME);

  // connect
  if(connect(socket_fd, reinterpret_cast<sockaddr *>(&addr), sizeof(addr)) < 0){
    perror("Socket connect error.");
    return -1;
  }

  // read data.

  for(;;){
    char buffer[128];
    printf("Please input a message: ");
    ssize_t read_number = read(0, buffer, 128);
    if(read_number < 0){
      perror("Get input error.");
      return -1;
    }
    if(write(socket_fd, buffer, read_number) < 0){
      perror("Socket write error.");
      return -1;
    }
    // wait for reply
    bzero(buffer, 128);
    ssize_t reply_number = read(socket_fd, buffer, 128);
    if(reply_number < 0){
      perror("Read error.");
    }
    printf("Reply nessage: %s \n", buffer);
  }

  return 0;
}