#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "ion_runtime.h"

extern int write(int fd, uint8_t* buf, int count);

int main(void);
void io_print(ion_string_t* s);
void io_println(ion_string_t* s);
void io_print_str(uint8_t* s, int len);
void io_print_int(int n);
int main(void) {
    int ret_val = 0;
    io_println(ion_string_from_literal("Hello, World!", 13));
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

void io_print(ion_string_t* s) {
    {
        int _result = write(1, s->data, (int)s->len);
    }
epilogue:
        return;
}

void io_println(ion_string_t* s) {
    {
        int _result = write(1, s->data, (int)s->len);
        int _newline = write(1, "\n", 1);
    }
epilogue:
        return;
}

void io_print_str(uint8_t* s, int len) {
    if (len < 0) {
                goto epilogue;
    }
    {
        int _result = write(1, s, len);
    }
epilogue:
        return;
}

void io_print_int(int n) {
    uint8_t buf[12] = {0};
    int len = 0;
    int negative = 0;
    int value = n;
    if (value == 0) {
        {
            int _result = write(1, "0", 1);
        }
                goto epilogue;
    }
    if (value < 0) {
        negative = 1;
        if (value == (-2147483648)) {
            {
                int _result = write(1, "-2147483648", 11);
            }
                        goto epilogue;
        }
        value = (0 - value);
    }
    while (value > 0) {
        int digit = (value % 10);
        buf[len] = (uint8_t)(48 + digit);
        len = (len + 1);
        value = (value / 10);
    }
    int i = 0;
    while (i < (len / 2)) {
        uint8_t tmp = ({ int __ion_idx_0 = i; (__ion_idx_0 >= 0 && __ion_idx_0 < 12) ? buf[__ion_idx_0] : (ion_panic("Array index out of bounds"), buf[0]); });
        buf[i] = ({ int __ion_idx_1 = ((len - 1) - i); (__ion_idx_1 >= 0 && __ion_idx_1 < 12) ? buf[__ion_idx_1] : (ion_panic("Array index out of bounds"), buf[0]); });
        buf[((len - 1) - i)] = tmp;
        i = (i + 1);
    }
    if (negative) {
        {
            int _minus = write(1, "-", 1);
        }
    }
    {
        int _result = write(1, &buf[0], len);
    }
epilogue:
        return;
}

