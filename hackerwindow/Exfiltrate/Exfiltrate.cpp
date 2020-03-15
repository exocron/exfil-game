#include <Windows.h>
#include <iostream>
#include <stdexcept>
#include <string>

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

int main()
{
	try {
		init_wsa();
		init_console();
		std::cout << "Hello \x1b[31mWorld\x1b[0m!\n";
	} catch (const std::runtime_error& e) {
		std::cout << e.what() << std::endl;
		std::cout << "Can't continue\n";
	}
	system("pause");
}
