#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf("%c", ch)
#define MEMORY 34

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
    memory[0] = 72;
    memory[1] = 101;
    memory[2] = 108;
    memory[3] = 108;
    memory[4] = 111;
    memory[5] = 32;
    memory[6] = 87;
    memory[7] = 111;
    memory[8] = 114;
    memory[9] = 108;
    memory[10] = 100;
    memory[11] = 33;
    memory[12] = 10;
    memory[13] = 0;
    memory[14] = 0;
    memory[15] = memory[memory[14]];
    memory[16] = memory[15] != 0;
    memory[17] = memory[16];
    while (memory[17]) {
    memory[18] = memory[memory[14]];
    OUT(memory[18]);
    ++memory[14];
    memory[19] = memory[memory[14]];
    memory[20] = memory[19] != 0;
    memory[17] = memory[20];
    }
    memory[21] = 68;
    memory[22] = 111;
    memory[23] = 110;
    memory[24] = 101;
    memory[25] = 10;
    memory[26] = 0;
    memory[27] = 21;
    memory[28] = memory[memory[27]];
    memory[29] = memory[28] != 0;
    memory[30] = memory[29];
    while (memory[30]) {
    memory[31] = memory[memory[27]];
    OUT(memory[31]);
    ++memory[27];
    memory[32] = memory[memory[27]];
    memory[33] = memory[32] != 0;
    memory[30] = memory[33];
    }
}