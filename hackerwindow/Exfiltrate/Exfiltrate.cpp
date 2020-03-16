#include <Windows.h>
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

void init_websocket()
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
		init_websocket();
		std::cout << "Hello \x1b[31mWorld\x1b[0m!\n";
	} catch (const std::runtime_error& e) {
		std::cout << e.what() << std::endl;
		std::cout << "Can't continue\n";
	}
	system("pause");
	return 0;
}
