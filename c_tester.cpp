#include <cstdio>
#include <cstdlib>

inline double r2() {
    return (double)rand() * 10.0/ (double)RAND_MAX;
}

int main() {
    srand(33333);
    for (int i = 0; i < 100; i++) {
        double r = r2();
        printf("%.6lf %d\n", r, (int) r);
    }
    return 0;
}
