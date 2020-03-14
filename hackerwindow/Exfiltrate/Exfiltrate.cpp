// Exfiltrate.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <Windows.h>
#include <iostream>

int main()
{
	HANDLE std = GetStdHandle(STD_OUTPUT_HANDLE);
	if (std == INVALID_HANDLE_VALUE) {
		std::cout << "Can't enable terminal colors. A lot of stuff probably won't make sense :(\n";
		goto skip;
	}
	DWORD mode;
	if (!GetConsoleMode(std, &mode)) {
		std::cout << "Can't enable terminal colors. A lot of stuff probably won't make sense :(\n";
		goto skip;
	}
	mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING; // enable VT100 control characters
	if (!SetConsoleMode(std, mode)) {
		std::cout << "Can't enable terminal colors. A lot of stuff probably won't make sense :(\n";
	}

skip:
	std::cout << "Hello \x1b[31mWorld\x1b[0m!\n";
	system("pause");
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
