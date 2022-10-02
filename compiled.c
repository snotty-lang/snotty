#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf("%c", ch)
#define MEMORY 1

int in() {
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
    uint8_t memory[MEMORY];
}