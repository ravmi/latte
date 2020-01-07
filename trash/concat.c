#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* concat(char* a, char* b) {
    long int la = strlen(a);
    long int lb = strlen(b);
    char *c = malloc(la + lb + 1);
    strcpy(c, b);
    strcpy(c + lb, a);
    return c;
}
