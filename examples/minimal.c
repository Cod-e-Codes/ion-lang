#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "ion_runtime.h"

int main(void) {
    int ret_val = 0;
    int x = 10;
    int y = 20;
    int z = (x + y);
    ret_val = z;
    goto epilogue;
epilogue:
        return ret_val;
}

