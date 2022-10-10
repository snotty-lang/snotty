#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf("%c", ch)
#define DEBUG(len) for (int i = 0; i < len; i++) printf("%d ", memory[i]); OUT(10);

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

void fx(uint8_t *memory, uint8_t start, uint8_t fx_code) {
    switch (fx_code) {
        case 0:
            memory[1 + start] = memory[memory[0 + start]];
            memory[2 + start] = memory[1 + start] != 0;
            memory[3 + start] = memory[2 + start];
            while (memory[3 + start]) {
            memory[4 + start] = memory[memory[0 + start]];
            OUT(memory[4 + start]);
            ++memory[0 + start];
            memory[5 + start] = memory[memory[0 + start]];
            memory[6 + start] = memory[5 + start] != 0;
            memory[3 + start] = memory[6 + start];
            }
            break;

    }
}

int main() {
    uint8_t memory[420];
    uint8_t start = 0;
    memory[0 + start] = 72;
    memory[1 + start] = 101;
    memory[2 + start] = 121;
    memory[3 + start] = 32;
    memory[4 + start] = 98;
    memory[5 + start] = 111;
    memory[6 + start] = 98;
    memory[7 + start] = 33;
    memory[8 + start] = 10;
    memory[9 + start] = 0;
    memory[10 + start] = 0 + start;
    memory[11 + start] = memory[memory[10 + start]];
    memory[12 + start] = memory[11 + start] != 0;
    memory[13 + start] = memory[12 + start];
    while (memory[13 + start]) {
    memory[14 + start] = memory[memory[10 + start]];
    OUT(memory[14 + start]);
    ++memory[10 + start];
    memory[15 + start] = memory[memory[10 + start]];
    memory[16 + start] = memory[15 + start] != 0;
    memory[13 + start] = memory[16 + start];
    }
    memory[17 + start] = 0;
    memory[18 + start] = 72;
    memory[19 + start] = 111;
    memory[20 + start] = 119;
    memory[21 + start] = 39;
    memory[22 + start] = 115;
    memory[23 + start] = 32;
    memory[24 + start] = 105;
    memory[25 + start] = 116;
    memory[26 + start] = 32;
    memory[27 + start] = 103;
    memory[28 + start] = 111;
    memory[29 + start] = 105;
    memory[30 + start] = 110;
    memory[31 + start] = 103;
    memory[32 + start] = 63;
    memory[33 + start] = 10;
    memory[34 + start] = 0;
    memory[35 + start] = 18 + start;
    fx(memory, 35 + start, memory[17 + start]);
}
