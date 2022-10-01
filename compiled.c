\
#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf("%c", ch)
#define MEMORY 4

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
    memory[0] = 32;
    memory[1] = memory[0] < 128;
    memory[2] = memory[1];
    while (memory[2]) {
    OUT(memory[0]);
    ++memory[0];
    memory[3] = memory[0] < 128;
    memory[2] = memory[3];
    }
}