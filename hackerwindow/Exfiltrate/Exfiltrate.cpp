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

void process_stdin()
{
	INPUT_RECORD record;
	DWORD unused;
	if (ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), &record, 1, &unused) == 0) {
		throw std::runtime_error("ReadConsoleInput failed: " + std::to_string(GetLastError()));
	}
	// TODO: something
}

void process_websocket(int s)
{
	while (websocket_data_available(s) > 0) {
		unsigned char* buf;
		int len;
		int err = websocket_read_next_packet(s, &buf, &len);
		if (err < 0) {
			throw std::runtime_error("process_websocket failed: " + std::to_string(err));
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
			std::cout << result << std::endl;
			switch (result) {
			case WSA_WAIT_EVENT_0:
				// do something with stdin
				process_stdin();
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
