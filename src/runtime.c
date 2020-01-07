#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void printInt(long int a) {
    printf("%ld\n", a);
}

void printString(char* a) {
    printf("%s\n", a);
}

char* readString() {
    char* b = malloc(200);
    scanf("%s", b);
    return b;
}

long int readInt() {
    long int val;
    scanf("%ld", &val);
    return val;
}

char* _concat(char* a, char* b) {
    long int la = strlen(a);
    long int lb = strlen(b);
    char *c = malloc(la + lb + 1);
    strcpy(c, b);
    strcpy(c + lb, a);
    return c;
}

void error() {
    fprintf(stderr, "runtime error\n");
    exit(1);
}
