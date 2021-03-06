#define WIN32_LEAN_AND_MEAN

#include <winsock2.h>
#include <Ws2tcpip.h>

#include <iostream>
#include <stdexcept>
#include <string>

#include "config.h"
#include "websocket.h"

#pragma comment(lib, "Ws2_32.lib")

void init_wsa()
{
	WSADATA wsaData = {0};

	int result = WSAStartup(MAKEWORD(2, 2), &wsaData);
	if (result != 0) {
		throw std::runtime_error("WSAStartup failed: " + std::to_string(result));
	}
}

void init_console()
{
	HANDLE std = GetStdHandle(STD_OUTPUT_HANDLE);
	if (std == INVALID_HANDLE_VALUE) {
		throw std::runtime_error("GetStdHandle failed: " + std::to_string(GetLastError()));
	}
	DWORD mode;
	if (!GetConsoleMode(std, &mode)) {
		throw std::runtime_error("GetConsoleMode failed: " + std::to_string(GetLastError()));
	}
	mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING; // enable VT100 control characters
	if (!SetConsoleMode(std, mode)) {
		throw std::runtime_error("SetConsoleMode failed: " + std::to_string(GetLastError()));
	}
}

int init_websocket()
{
	int s = socket(AF_INET, SOCK_STREAM, 0);
	if (s == INVALID_SOCKET) {
		throw std::runtime_error("socket failed: " + std::to_string(WSAGetLastError()));
	}
	sockaddr_in addr = {0};
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(GAME_SERVER);
	addr.sin_port = htons(GAME_SERVER_PORT);
	if (connect(s, (sockaddr*)&addr, sizeof(addr))) {
		throw std::runtime_error("connect failed: " + std::to_string(WSAGetLastError()));
	}
	int err;
	if (err = websocket_connect(s)) {
		throw std::runtime_error("websocket_connect failed: " + std::to_string(err));
	}
	if (err = websocket_authenticate(s)) {
		throw std::runtime_error("websocket_authenticate failed: " + std::to_string(err));
	}
	return s;
}

void process_stdin_buffered(int s)
{
	static char buf[256] = {0};
	static unsigned char len = 0;
	INPUT_RECORD record[32];
	DWORD event_length;
	HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
	while (true) {
		if (GetNumberOfConsoleInputEvents(in, &event_length) == 0) {
			throw std::runtime_error("GetNumberOfConsoleInputEvents failed: " + std::to_string(GetLastError()));
		}
		if (event_length == 0) {
			return;
		}
		if (ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), record, sizeof(record)/sizeof(*record), &event_length) == 0) {
			throw std::runtime_error("ReadConsoleInput failed: " + std::to_string(GetLastError()));
		}
		for (int i = 0; i < event_length; i++) {
			if (record[i].EventType == KEY_EVENT && record[i].Event.KeyEvent.bKeyDown) {
				char c = record[i].Event.KeyEvent.uChar.AsciiChar;
				if (c) {
					if (c == 27) { // escape
						continue;
					} else if (c == 8) { // backspace
						if (len != 0) {
							buf[--len] = 0;
							std::cout << "\x1b[D \x1b[D" << std::flush;
						}
					} else if (c == 13) { // return
						// TODO: write to websocket
						std::cout << std::endl;
						int err = websocket_write_packet(s, buf, len);
						if (err) {
							throw std::runtime_error("websocket_write_packet failed: " + std::to_string(err));
						}
						memset(buf, 0, sizeof(buf));
						len = 0;
					} else {
						buf[len++] = c;
						std::cout << c << std::flush;
					}
				}
			}
		}
	}
}

void process_websocket(int s)
{
	while (websocket_data_available(s) > 0) {
		unsigned char* buf;
		int len;
		int err = websocket_read_next_packet(s, &buf, &len);
		if (err < 0) {
			throw std::runtime_error("websocket_read_next_packet failed: " + std::to_string(err));
		}
		if (err == 1) {
			buf[len] = 0; // TODO: fix
			std::cout << (char*)buf;
		}
	}
}

int main(int argc, char** argv)
{
	if (argc <= 2) {
		std::cout << "Not enough arguments\n";
		system("pause");
		return 0;
	}
	websocket_game_code = argv[1];
	websocket_game_token = argv[2];
	try {
		init_wsa();
		init_console();
		int s = init_websocket();
		std::cout << "Opening a remote shell to [REDACTED]...\nConnected.\n\n";

		HANDLE handles[] = {
			GetStdHandle(STD_INPUT_HANDLE),
			WSACreateEvent(),
		};
		WSANETWORKEVENTS events;
		WSAEventSelect(s, handles[1], FD_READ);
		while (true) {
			DWORD result = WSAWaitForMultipleEvents(
				sizeof(handles)/sizeof(*handles),
				handles,
				FALSE,
				WSA_INFINITE,
				TRUE
			);
			switch (result) {
			case WSA_WAIT_EVENT_0:
				process_stdin_buffered(s);
				break;
			case WSA_WAIT_EVENT_0 + 1:
				WSAEnumNetworkEvents(s, handles[1], &events);
				process_websocket(s);
				break;
			default:
				break;
			}
		}
	} catch (const std::runtime_error& e) {
		std::cout << e.what() << std::endl;
		std::cout << "Can't continue\n";
	}
	system("pause");
	return 0;
}
