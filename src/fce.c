#include <stdio.h>

int writeln_int(int x) {
    printf("%d\n", x);
    return 0;
}
int writeln_string(const char * x) {
    printf("%s\n", x);
    return 0;
}
int write_int(int x) {
    printf("%d", x);
    return 0;
}
int write_string(const char * x) {
    printf("%s", x);
    return 0;
}
int readln_int(int *x) {
    scanf("%d", x);
    return 0;
}
int dec_int(int * x) {
    return --*x;
}
