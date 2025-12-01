#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "ion_runtime.h"

extern int write(int fd, uint8_t* buf, int count);

int main(void) {
    int ret_val = 0;
    {
        int _result = write(1, "Hello, World!\n", 14);
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

