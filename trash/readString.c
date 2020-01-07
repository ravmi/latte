#include <stdlib.h>
#include <stdio.h>

char* readString() {
    char* b = malloc(200);
    scanf("%s", b);
    return b;
}
