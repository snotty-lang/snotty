#include <stdio.h>
#include <termios.h>

int getch() {
    struct termios old, new;
    int ch;
    tcgetattr(0, &old);
    new = old;
    new.c_lflag &= ~ICANON;
    new.c_lflag &= ~ECHO;
    tcsetattr(0, TCSANOW, &new);
    ch = getchar();
    tcsetattr(0, TCSANOW, &old);
    return ch;
}

int main() {
    unsigned char mem[30000];
    unsigned char* ptr = mem + 0;

	unsigned int debug_count = 0;
	*ptr = 0;
	ptr += 2;
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	ptr += 2;
	while (*ptr) {
	*ptr += -1;
	ptr += -2;
	*ptr += 1;
	ptr += 2;
	}
	*ptr = 100;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 1;
	ptr += 1;
	*ptr = 0;
	ptr += -4;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	*ptr += -1;
	}
	ptr += -2;
	while (*ptr) {
	ptr += 2;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	*ptr += 1;
	ptr += 3;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -4;
	*ptr += -1;
	ptr += 2;
	*ptr = 0;
	ptr += 2;
	*ptr += -1;
	ptr += 1;
	}
	ptr += -1;
	*ptr += 1;
	ptr += -2;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -4;
	*ptr += -1;
	ptr += 2;
	*ptr = 1;
	ptr += 2;
	*ptr += -1;
	ptr += 1;
	}
	ptr += -1;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	ptr += -1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += -5;
	while (*ptr) {
	ptr += 1;
	*ptr = 1;
	ptr += 3;
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	ptr += -4;
	while (*ptr) {
	ptr += 4;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -5;
	*ptr += -1;
	}
	ptr += 5;
	while (*ptr) {
	*ptr += -1;
	ptr += -5;
	*ptr += 1;
	ptr += 5;
	}
	*ptr = 3;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	ptr += 1;
	*ptr += 1;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	*ptr += -1;
	}
	ptr += -2;
	while (*ptr) {
	ptr += -1;
	}
	ptr += 1;
	*ptr += -1;
	}
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	while (*ptr) {
	*ptr += -1;
	ptr += -3;
	*ptr += 1;
	ptr += 3;
	}
	ptr += -1;
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	ptr += 2;
	while (*ptr) {
	*ptr += -1;
	ptr += -2;
	*ptr += 1;
	ptr += 2;
	}
	*ptr = 0;
	ptr += -1;
	while (*ptr) {
	*ptr += -1;
	ptr += 1;
	*ptr += -1;
	ptr += -1;
	}
	*ptr += 1;
	ptr += 1;
	while (*ptr) {
	ptr += -1;
	*ptr += -1;
	ptr += 1;
	*ptr = 0;
	}
	ptr += 2;
	*ptr = 0;
	ptr += -2;
	*ptr = 0;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 2;
	*ptr += 1;
	ptr += -3;
	*ptr += -1;
	}
	ptr += 3;
	while (*ptr) {
	*ptr += -1;
	ptr += -3;
	*ptr += 1;
	ptr += 3;
	}
	ptr += -3;
	while (*ptr) {
	ptr += 3;
	*ptr = 102;
	putchar(*ptr);
	*ptr = 105;
	putchar(*ptr);
	*ptr = 122;
	putchar(*ptr);
	*ptr = 122;
	putchar(*ptr);
	*ptr = 0;
	ptr += -5;
	*ptr = 0;
	ptr += 3;
	*ptr = 0;
	}
	ptr += 4;
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	ptr += -8;
	while (*ptr) {
	ptr += 8;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -9;
	*ptr += -1;
	}
	ptr += 9;
	while (*ptr) {
	*ptr += -1;
	ptr += -9;
	*ptr += 1;
	ptr += 9;
	}
	*ptr = 5;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	ptr += 1;
	*ptr += 1;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	*ptr += -1;
	}
	ptr += -2;
	while (*ptr) {
	ptr += -1;
	}
	ptr += 1;
	*ptr += -1;
	}
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	while (*ptr) {
	*ptr += -1;
	ptr += -3;
	*ptr += 1;
	ptr += 3;
	}
	ptr += -1;
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	ptr += 2;
	while (*ptr) {
	*ptr += -1;
	ptr += -2;
	*ptr += 1;
	ptr += 2;
	}
	*ptr = 0;
	ptr += -1;
	while (*ptr) {
	*ptr += -1;
	ptr += 1;
	*ptr += -1;
	ptr += -1;
	}
	*ptr += 1;
	ptr += 1;
	while (*ptr) {
	ptr += -1;
	*ptr += -1;
	ptr += 1;
	*ptr = 0;
	}
	ptr += 2;
	*ptr = 0;
	ptr += -2;
	*ptr = 0;
	ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 2;
	*ptr += 1;
	ptr += -3;
	*ptr += -1;
	}
	ptr += 3;
	while (*ptr) {
	*ptr += -1;
	ptr += -3;
	*ptr += 1;
	ptr += 3;
	}
	ptr += -3;
	while (*ptr) {
	ptr += 3;
	*ptr = 98;
	putchar(*ptr);
	*ptr = 117;
	putchar(*ptr);
	*ptr = 122;
	putchar(*ptr);
	*ptr = 122;
	putchar(*ptr);
	*ptr = 0;
	ptr += -9;
	*ptr = 0;
	ptr += 7;
	*ptr = 0;
	}
	ptr += 4;
	*ptr = 0;
	ptr += -2;
	*ptr = 0;
	ptr += -9;
	while (*ptr) {
	ptr += 9;
	*ptr += 1;
	ptr += 2;
	*ptr += 1;
	ptr += -11;
	*ptr += -1;
	}
	ptr += 11;
	while (*ptr) {
	*ptr += -1;
	ptr += -11;
	*ptr += 1;
	ptr += 11;
	}
	ptr += -1;
	*ptr += 1;
	ptr += -1;
	while (*ptr) {
	ptr += 3;
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	ptr += -13;
	while (*ptr) {
	ptr += 13;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -14;
	*ptr += -1;
	}
	ptr += 14;
	while (*ptr) {
	*ptr += -1;
	ptr += -14;
	*ptr += 1;
	ptr += 14;
	}
	ptr += 1;
	*ptr += 10;
	ptr += -2;
	while (*ptr) {
	*ptr += -1;
	ptr += 1;
	*ptr += 1;
	ptr += 1;
	*ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 2;
	}
	ptr += 1;
	while (*ptr) {
	*ptr += 1;
	while (*ptr) {
	*ptr += -1;
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	}
	ptr += 1;
	*ptr += 1;
	ptr += 2;
	}
	ptr += -6;
	}
	ptr += 2;
	*ptr = 0;
	ptr += 3;
	*ptr += 10;
	ptr += -1;
	while (*ptr) {
	*ptr += -1;
	ptr += 1;
	*ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 2;
	}
	ptr += 1;
	while (*ptr) {
	*ptr += 1;
	while (*ptr) {
	*ptr += -1;
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	}
	ptr += 1;
	*ptr += 1;
	ptr += 2;
	}
	ptr += -5;
	}
	ptr += 1;
	*ptr = 0;
	ptr += 2;
	while (*ptr) {
	ptr += 1;
	*ptr += 6;
	while (*ptr) {
	*ptr += -1;
	ptr += -1;
	*ptr += 8;
	ptr += 1;
	}
	ptr += -1;
	putchar(*ptr);
	ptr += -2;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += 1;
	*ptr = 0;
	}
	ptr += -1;
	while (*ptr) {
	ptr += -1;
	while (*ptr) {
	*ptr += -1;
	ptr += 1;
	*ptr += -1;
	ptr += -1;
	}
	*ptr += 6;
	while (*ptr) {
	*ptr += -1;
	ptr += 1;
	*ptr += 8;
	ptr += -1;
	}
	ptr += 1;
	putchar(*ptr);
	*ptr = 0;
	}
	ptr += -2;
	*ptr += 6;
	while (*ptr) {
	*ptr += -1;
	ptr += -1;
	*ptr += 8;
	ptr += 1;
	}
	ptr += -1;
	putchar(*ptr);
	*ptr = 0;
	ptr += -2;
	while (*ptr) {
	*ptr += -1;
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	}
	ptr += -1;
	*ptr = 32;
	putchar(*ptr);
	*ptr = 10;
	putchar(*ptr);
	*ptr = 0;
	ptr += -1;
	*ptr += -1;
	ptr += -1;
	*ptr = 0;
	}
	ptr += 1;
	while (*ptr) {
	ptr += 1;
	*ptr = 10;
	putchar(*ptr);
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	}
	ptr += -10;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += -12;
	*ptr += 1;
	ptr += 3;
	*ptr = 0;
	ptr += -1;
	*ptr = 0;
	ptr += -2;
	while (*ptr) {
	ptr += 2;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -3;
	*ptr += -1;
	}
	ptr += 3;
	while (*ptr) {
	*ptr += -1;
	ptr += -3;
	*ptr += 1;
	ptr += 3;
	}
	*ptr = 100;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 1;
	ptr += 1;
	*ptr = 0;
	ptr += -4;
	while (*ptr) {
	ptr += 1;
	*ptr += 1;
	ptr += 1;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	*ptr += -1;
	}
	ptr += -2;
	while (*ptr) {
	ptr += 2;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	*ptr += 1;
	ptr += 3;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -4;
	*ptr += -1;
	ptr += 2;
	*ptr = 0;
	ptr += 2;
	*ptr += -1;
	ptr += 1;
	}
	ptr += -1;
	*ptr += 1;
	ptr += -2;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	while (*ptr) {
	ptr += 1;
	*ptr += -1;
	}
	ptr += 1;
	while (*ptr) {
	ptr += -4;
	*ptr += -1;
	ptr += 2;
	*ptr = 1;
	ptr += 2;
	*ptr += -1;
	ptr += 1;
	}
	ptr += -1;
	*ptr += 1;
	ptr += -2;
	*ptr += -1;
	}
	ptr += -1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += -4;
	*ptr = 0;
	ptr += -2;
	*ptr = 0;
	ptr += 1;
	while (*ptr) {
	ptr += -1;
	*ptr += 1;
	ptr += 2;
	*ptr += 1;
	ptr += -1;
	*ptr += -1;
	}
	ptr += 1;
	while (*ptr) {
	*ptr += -1;
	ptr += -1;
	*ptr += 1;
	ptr += 1;
	}
	ptr += -2;
	}
	ptr += -1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += 1;
	*ptr = 0;
	ptr += -1;
	return 0;
}
