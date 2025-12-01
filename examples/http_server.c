#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "ion_runtime.h"

typedef struct SockAddrInBytes {
    uint8_t data[16];
} SockAddrInBytes;

extern int socket(int domain, int sock_type, int protocol);
extern int bind(int sockfd, uint8_t* addr, int addrlen);
extern int listen(int sockfd, int backlog);
extern int accept(int sockfd, uint8_t* addr, int* addrlen);
extern int recv_sys(int sockfd, uint8_t* buf, int len, int flags);
extern int send_sys(int sockfd, uint8_t* buf, int len, int flags);
extern int close(int fd);
extern uint16_t htons(uint16_t hostshort);

SockAddrInBytes create_sockaddr_in(uint16_t port) {
    SockAddrInBytes ret_val = {0};
    {
        uint16_t port_net = htons(port);
        uint16_t shift8 = 8;
        uint8_t port_high = (uint8_t)(port_net >> shift8);
        uint16_t mask_255 = 255;
        uint8_t port_low = (uint8_t)(port_net & mask_255);
        uint8_t addr[16] = {(uint8_t)2, (uint8_t)0, port_high, port_low, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0, (uint8_t)0};
        ret_val = (SockAddrInBytes){{0}} /* ARRAY_FIELD:data:addr */;
        memcpy(&ret_val.data, &addr, sizeof(ret_val.data));
        goto epilogue;
    }
    goto epilogue;
epilogue:
        return ret_val;
}

int handle_client(int client_fd) {
    int ret_val = 0;
    uint8_t buffer[128] = {0};
    {
        int received = recv_sys(client_fd, &buffer[0], 128, 0);
        if (received < 1) {
            close(client_fd);
            ret_val = 0;
            goto epilogue;
        }
        int _sent1 = send_sys(client_fd, "HTTP/1.1 200 OK\r\n", 17, 0);
        int _sent2 = send_sys(client_fd, "Content-Type: text/html\r\n", 25, 0);
        int _sent3 = send_sys(client_fd, "Content-Length: 89\r\n", 20, 0);
        int _sent4 = send_sys(client_fd, "Connection: close\r\n", 19, 0);
        int _sent5 = send_sys(client_fd, "\r\n", 2, 0);
        int _sent6 = send_sys(client_fd, "<html><head><title>Ion HTTP Server</title></head><body><h1>Hello from Ion!</h1><p>This is a simple HTTP server written in Ion.</p></body></html>", 89, 0);
        close(client_fd);
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int main(void) {
    int ret_val = 0;
    {
        int server_fd = socket(2, 1, 0);
        if (server_fd < 0) {
            ret_val = 1;
            goto epilogue;
        }
        SockAddrInBytes sockaddr = create_sockaddr_in(8080);
        int bind_result = bind(server_fd, &sockaddr.data[0], 16);
        if (bind_result < 0) {
            close(server_fd);
            ret_val = 2;
            goto epilogue;
        }
        int listen_result = listen(server_fd, 10);
        if (listen_result < 0) {
            close(server_fd);
            ret_val = 3;
            goto epilogue;
        }
        while (1) {
            uint8_t client_addr[16] = {0};
            int addrlen = 16;
            int client_fd = accept(server_fd, &client_addr[0], &addrlen);
            int _result = handle_client(client_fd);
        }
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

