#include <stdio.h>

int writeln_int(int x) {
    printf("%d\n", x);
    return 0;
}
int writeln_string(const char * x) {
    printf("%s\n", x);
    return 0;
}
int writeln_double(double x) {
    printf("%lf\n", x);
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
int write_double(double x) {
    printf("%lf", x);
    return 0;
}
int readln_int__ref(int * x) {
    scanf("%d", x);
    return 0;
}
int readln_double__ref(double * x) {
    scanf("%lf", x);
    return 0;
}
int dec_int__ref(int * x) {
    return --*x;
}
int dec_double__ref(double * x) {
    return --*x;
}
