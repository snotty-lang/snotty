#include <stdio.h>
#include <stdint.h>
#include <termios.h>
#define OUT(ch) printf("%c", ch)
#define MEMORY 65

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
    memory[21] = 0;
    memory[22] = 0;
    memory[23] = memory[22] < 8;
    memory[24] = memory[23];
    while (memory[24]) {
    memory[25] = 72;
    memory[26] = 105;
    memory[27] = 32;
    memory[28] = 109;
    memory[29] = 97;
    memory[30] = 110;
    memory[31] = 33;
    memory[32] = 10;
    memory[33] = 0;
    memory[34] = 25;
    memory[35] = memory[22];
    memory[37] = memory[34] + memory[35];
    memory[38] = memory[memory[37]];
    memory[36] = memory[38];
    OUT(memory[36]);
    ++memory[22];
    memory[25] = memory[22] < 8;
    memory[24] = memory[25];
    }
    memory[26] = 0;
    memory[27] = memory[26] < 8;
    memory[28] = memory[27];
    while (memory[28]) {
    memory[29] = 72;
    memory[30] = 105;
    memory[31] = 32;
    memory[32] = 109;
    memory[33] = 97;
    memory[34] = 110;
    memory[35] = 33;
    memory[36] = 10;
    memory[37] = 0;
    memory[38] = 29;
    memory[39] = memory[26];
    memory[41] = memory[38] + memory[39];
    memory[42] = memory[memory[41]];
    memory[40] = memory[42];
    OUT(memory[40]);
    ++memory[26];
    memory[29] = memory[26] < 8;
    memory[28] = memory[29];
    }
    memory[30] = 0;
    memory[31] = memory[30] < 8;
    memory[32] = memory[31];
    while (memory[32]) {
    memory[33] = 72;
    memory[34] = 105;
    memory[35] = 32;
    memory[36] = 109;
    memory[37] = 97;
    memory[38] = 110;
    memory[39] = 33;
    memory[40] = 10;
    memory[41] = 0;
    memory[42] = 33;
    memory[43] = memory[30];
    memory[45] = memory[42] + memory[43];
    memory[46] = memory[memory[45]];
    memory[44] = memory[46];
    OUT(memory[44]);
    ++memory[30];
    memory[33] = memory[30] < 8;
    memory[32] = memory[33];
    }
    memory[34] = 0;
    memory[35] = memory[34] < 8;
    memory[36] = memory[35];
    while (memory[36]) {
    memory[37] = 72;
    memory[38] = 105;
    memory[39] = 32;
    memory[40] = 109;
    memory[41] = 97;
    memory[42] = 110;
    memory[43] = 33;
    memory[44] = 10;
    memory[45] = 0;
    memory[46] = 37;
    memory[47] = memory[34];
    memory[49] = memory[46] + memory[47];
    memory[50] = memory[memory[49]];
    memory[48] = memory[50];
    OUT(memory[48]);
    ++memory[34];
    memory[37] = memory[34] < 8;
    memory[36] = memory[37];
    }
    memory[38] = 0;
    memory[39] = memory[38] < 8;
    memory[40] = memory[39];
    while (memory[40]) {
    memory[41] = 72;
    memory[42] = 105;
    memory[43] = 32;
    memory[44] = 109;
    memory[45] = 97;
    memory[46] = 110;
    memory[47] = 33;
    memory[48] = 10;
    memory[49] = 0;
    memory[50] = 41;
    memory[51] = memory[38];
    memory[53] = memory[50] + memory[51];
    memory[54] = memory[memory[53]];
    memory[52] = memory[54];
    OUT(memory[52]);
    ++memory[38];
    memory[41] = memory[38] < 8;
    memory[40] = memory[41];
    }
}