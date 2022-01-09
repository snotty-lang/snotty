#include <stdio.h>

int main(int argc, char const *argv[]) {
    unsigned char a = ~'1';
    unsigned char b = -'1';
    unsigned char c = '1';
    printf("%d %d %d\n",c, a, b);
    return 0;
}
