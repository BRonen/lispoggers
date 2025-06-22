#include <stdio.h>

int my_rust_function(int arg);

int main() {
    int result = my_rust_function(20);
    printf("Result from Rust: %d\n", result);
    return 0;
}
