#ifndef WEBSOCKET_H
#define WEBSOCKET_H

// this would normally be a separate c(pp) file, but VS isn't
// working right now, also I'm lazy

#include "config.h"

unsigned char websocket_buffer[4096];

const unsigned char websocket_key[] = {
	0x0B, 0x1E, 0x28, 0x44, 0x3F, 0x0F, 0xBC, 0xCB,
    0x97, 0xF4, 0x1C, 0xFF, 0x49, 0x54, 0xD8, 0x0D
};

const unsigned char websocket_mask[] = {
	0x01, 0x56, 0x58, 0x32
};

const char http_request[] = "GET /join HTTP/1.1\r\n"
    "Host: unused\r\n"
    "Upgrade: websocket\r\n"
    "Connection: Upgrade\r\n"
    "Sec-WebSocket-Key: Cx4oRD8PvMuX9Bz/SVTYDQ==\r\n"
    "Sec-WebSocket-Version: 13\r\n\r\n";

const char http_response[] = "HTTP/1.1 101 "; // hacky

const char _ws_game_join_1[] = "\x81\xB5\x00\x00\x00\x00{\"code\":\"";
const char _ws_game_join_2[] = "\",\"token\":\"";
const char _ws_game_join_3[] = "\"}";

char *websocket_game_code = NULL;
char *websocket_game_token = NULL;

int _ws_loop_write(int socket, const char *buf, int len, int flags)
{
    int rem = len;
    int r;
    while (rem > 0 && (r = send(socket, buf, len, flags)) > 0) {
        rem -= r;
    }
    if (r == SOCKET_ERROR || r == 0) {
        return SOCKET_ERROR;
    }
    return len;
}

int _ws_loop_read(int socket, char *buf, int len, int flags)
{
    int rem = len;
    int r;
    while (rem > 0 && (r = recv(socket, buf, len, flags)) > 0) {
        rem -= r;
    }
    if (r == SOCKET_ERROR || r == 0) {
        return SOCKET_ERROR;
    }
    return len;
}

int _ws_scan_newline(int socket)
{
    int r;
    int state = 0;
    while ((r = recv(socket, (char*)websocket_buffer, sizeof(websocket_buffer), 0)) > 0) {
        for (int i = 0; i < r; i++) {
            if (state & 1) {
                if (websocket_buffer[i] == '\n') {
                    state++;
                } else {
                    state = 0;
                }
            } else {
                if (websocket_buffer[i] == '\r') {
                    state++;
                } else {
                    state = 0;
                }
            }
            if (state == 4) {
                return 0;
            }
        }
    }
    if (r == SOCKET_ERROR) {
        return SOCKET_ERROR;
    }
    return -1;
}

int websocket_connect(int socket)
{
    int err = _ws_loop_write(socket, http_request, sizeof(http_request) - 1, 0);
    if (err == SOCKET_ERROR) {
        return -1;
    }
    err = _ws_loop_read(socket, (char*)websocket_buffer, sizeof(http_response) - 1, 0);
    if (err == SOCKET_ERROR) {
        return -2;
    }
    if (memcmp(websocket_buffer, http_response, sizeof(http_response) - 1)) {
        return -3;
    }
    if (_ws_scan_newline(socket)) {
        return -4;
    }
    return 0;
}

int websocket_authenticate(int socket)
{
    int err = _ws_loop_write(socket, _ws_game_join_1, sizeof(_ws_game_join_1) - 1, 0);
    if (err == SOCKET_ERROR) {
        return -1;
    }
    err = _ws_loop_write(socket, websocket_game_code, 9, 0);
    if (err == SOCKET_ERROR) {
        return -2;
    }
    err = _ws_loop_write(socket, _ws_game_join_2, sizeof(_ws_game_join_2) - 1, 0);
    if (err == SOCKET_ERROR) {
        return -3;
    }
    err = _ws_loop_write(socket, websocket_game_token, 22, 0);
    if (err == SOCKET_ERROR) {
        return -4;
    }
    err = _ws_loop_write(socket, _ws_game_join_3, sizeof(_ws_game_join_3) - 1, 0);
    if (err == SOCKET_ERROR) {
        return -5;
    }
    return 0;
}

int websocket_data_available(int socket)
{
    unsigned long l;
    if (ioctlsocket(socket, FIONREAD, &l) == SOCKET_ERROR) {
        return -WSAGetLastError();
    }
    return (int)l;
}

int websocket_read_next_packet(int socket, unsigned char **buf, int *len)
{
    unsigned long long l = 0;
    int err = _ws_loop_read(socket, (char*)websocket_buffer, 2, 0);
    if (err == SOCKET_ERROR) {
        return -1;
    }
    if (websocket_buffer[1] & 0x80) {
        return -2; // fatal according to RFC 6455
    }
    switch (websocket_buffer[1]) {
    case 126:
        err = _ws_loop_read(socket, (char*)websocket_buffer + 2, 2, 0);
        if (err == SOCKET_ERROR) {
            return -3;
        }
        l = websocket_buffer[2] << 8 | websocket_buffer[3];
        break;
    case 127:
        if (sizeof(l) != 8) {
            return -128;
        }
        err = _ws_loop_read(socket, (char*)&l, 8, 0);
        if (err == SOCKET_ERROR) {
            return -4;
        }
        l = ntohll(l);
        if (l > sizeof(websocket_buffer)) {
            return -5;
        }
        break;
    default:
        l = websocket_buffer[1];
    }
    switch (websocket_buffer[0]) {
    case 0x88:
        return -6;
    case 0x81:
        err = _ws_loop_read(socket, (char*)websocket_buffer, l, 0);
        if (err == SOCKET_ERROR) {
            return -7;
        }
        return 0;
    case 0x82:
        err = _ws_loop_read(socket, (char*)websocket_buffer, l, 0);
        if (err == SOCKET_ERROR) {
            return -8;
        }
        *buf = websocket_buffer;
        *len = l;
        return 1;
    default:
        return -9; // TODO: fix
    }
}

int websocket_write_packet(int socket, const void *buf, int len)
{
    if (len > 125) {
        return -1; // TODO: fix
    }
    char header[6] = {0x82, 0x80 | (char)len, 0, 0, 0, 0};
    int err = _ws_loop_write(socket, header, sizeof(header), 0);
    if (err == SOCKET_ERROR) {
        return -2;
    }
    err = _ws_loop_write(socket, (const char *)buf, len, 0);
    if (err == SOCKET_ERROR) {
        return -3;
    }
    return 0;
}

#endif
