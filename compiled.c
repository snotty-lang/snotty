#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf("%c", ch)
#define MEMORY 11

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
    memory[0] = 1;
    memory[1] = memory[0] < 9;
    memory[2] = memory[1];
    while (memory[2]) {
    memory[3] = 0;
    memory[4] = memory[3] < memory[0];
    memory[5] = memory[4];
    while (memory[5]) {
    OUT(42);
    ++memory[3];
    memory[6] = memory[3] < memory[0];
    memory[5] = memory[6];
    }
    OUT(10);
    ++memory[0];
    memory[3] = memory[0] < 9;
    memory[2] = memory[3];
    }
}