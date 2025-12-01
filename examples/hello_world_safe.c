#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "ion_runtime.h"

extern int write(int fd, uint8_t* buf, int count);

int main(void);
void print(ion_string_t* s);
void println(ion_string_t* s);
void print_str(uint8_t* s, int len);
void print_int(int n);
int main(void) {
    int ret_val = 0;
    println(ion_string_from_literal("Hello, World!", 13));
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

void print(ion_string_t* s) {
    {
        int _result = write(1, s->data, (int)s->len);
    }
epilogue:
        return;
}

void println(ion_string_t* s) {
    {
        int _result = write(1, s->data, (int)s->len);
        int _newline = write(1, "\n", 1);
    }
epilogue:
        return;
}

void print_str(uint8_t* s, int len) {
    if (len < 0) {
                goto epilogue;
    }
    {
        int _result = write(1, s, len);
    }
epilogue:
        return;
}

void print_int(int n) {
    {
        if (n == 0) {
            int _result = write(1, "0", 1);
                        goto epilogue;
        }
        int digit = (n % 10);
        int char_code = (48 + digit);
        int _result = write(1, "TODO: full int conversion", 25);
    }
epilogue:
        return;
}

