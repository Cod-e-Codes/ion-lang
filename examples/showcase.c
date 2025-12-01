#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "ion_runtime.h"

typedef struct Point {
    int x;
    int y;
} Point;

typedef struct Pair_int {
    int first;
    int second;
} Pair_int;

typedef struct Option_int {
    int tag;
    union {
        struct {
            int arg0;
        } variant_0;
    } data;
} Option_int;

static Option_int Option_int_Some_new(int arg0) {
    Option_int result = { .tag = 0, .data = { .variant_0 = { .arg0 = arg0 } } };
    return result;
}

static Option_int Option_int_None_new() {
    Option_int result = { .tag = 1, .data = { } };
    return result;
}

typedef struct Result_int_int {
    int tag;
    union {
        struct {
            int arg0;
        } variant_0;
        struct {
            int arg0;
        } variant_1;
    } data;
} Result_int_int;

static Result_int_int Result_int_int_Ok_new(int arg0) {
    Result_int_int result = { .tag = 0, .data = { .variant_0 = { .arg0 = arg0 } } };
    return result;
}

static Result_int_int Result_int_int_Err_new(int arg0) {
    Result_int_int result = { .tag = 1, .data = { .variant_1 = { .arg0 = arg0 } } };
    return result;
}

typedef struct Vec_int {
    void* data;
    size_t len;
    size_t capacity;
    size_t elem_size;
} Vec_int;

int add(int x, int y) {
    int ret_val = 0;
    ret_val = (x + y);
    goto epilogue;
epilogue:
        return ret_val;
}

int multiply(int x, int y) {
    int ret_val = 0;
    ret_val = (x * y);
    goto epilogue;
epilogue:
        return ret_val;
}

Point create_point(int x, int y) {
    Point ret_val = {0};
    ret_val = (Point){.x = x, .y = y};
    goto epilogue;
epilogue:
        return ret_val;
}

int box_example(void) {
    int ret_val = 0;
    int value = 42;
    int* boxed = ({ int* ptr = (int*)ion_box_alloc(sizeof(int)); if (ptr) { *ptr = value; } ptr; });
    int unwrapped = (*boxed);
    ret_val = unwrapped;
    goto epilogue;
epilogue:
        return ret_val;
}

int vec_example(void) {
    int ret_val = 0;
    Vec_int* numbers = ((Vec_int*)(ion_vec_new(sizeof(int))));
    ion_vec_push((ion_vec_t*)(numbers), &((int){10}), sizeof(int));
    ion_vec_push((ion_vec_t*)(numbers), &((int){20}), sizeof(int));
    ion_vec_push((ion_vec_t*)(numbers), &((int){30}), sizeof(int));
    int len = ((numbers) ? (int)((ion_vec_t*)(numbers))->len : 0);
    if (len != 3) {
        ret_val = 1;
        goto epilogue;
    }
    Option_int match_val_0 = *((Option_int*)(ion_vec_get((ion_vec_t*)(numbers), 0, sizeof(int))));
    switch (match_val_0.tag) {
        case 0: // Some
            int value = match_val_0.data.variant_0.arg0;
            if (value != 10) {
                ret_val = 2;
                goto epilogue;
            }
            break;
        case 1: // None
            ret_val = 3;
            goto epilogue;
            break;
    }
    ion_vec_set((ion_vec_t*)(numbers), 1, &((int){25}), sizeof(int));
    Option_int match_val_1 = *((Option_int*)(ion_vec_pop((ion_vec_t*)(numbers), sizeof(int))));
    switch (match_val_1.tag) {
        case 0: // Some
            int value = match_val_1.data.variant_0.arg0;
            if (value != 30) {
                ret_val = 4;
                goto epilogue;
            }
            break;
        case 1: // None
            ret_val = 5;
            goto epilogue;
            break;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int string_example(void) {
    int ret_val = 0;
    ion_string_t* greeting = ion_string_new();
    ion_string_push_str(greeting, "Hello", 5);
    ion_string_push_str(greeting, ", ", 2);
    ion_string_push_str(greeting, "Ion!", 4);
    int len = ((greeting) ? (int)((ion_vec_t*)(greeting))->len : 0);
    if (len != 11) {
        ret_val = 1;
        goto epilogue;
    }
    ion_string_t* message = ion_string_from_literal("Welcome to Ion", 14);
    int msg_len = ((message) ? (int)((ion_vec_t*)(message))->len : 0);
    if (msg_len != 14) {
        ret_val = 2;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int if_example(void) {
    int ret_val = 0;
    int cap = 10;
    if (cap < 10) {
        ret_val = 1;
        goto epilogue;
    }
    int len = 5;
    if (len == 0) {
        ret_val = 2;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int while_example(void) {
    int ret_val = 0;
    int x = 0;
    while (x < 0) {
        int _ = (x + 1);
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int control_flow_example(void) {
    int ret_val = 0;
    if (if_example() != 0) {
        ret_val = 1;
        goto epilogue;
    }
    if (while_example() != 0) {
        ret_val = 2;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int pattern_matching_example(void) {
    int ret_val = 0;
    Option_int opt = Option_int_Some_new(42);
    Option_int match_val_2 = opt;
    switch (match_val_2.tag) {
        case 0: // Some
            int value = match_val_2.data.variant_0.arg0;
            if (value != 42) {
                ret_val = 1;
                goto epilogue;
            }
            break;
        case 1: // None
            ret_val = 2;
            goto epilogue;
            break;
    }
    Result_int_int result = Result_int_int_Ok_new(100);
    Result_int_int match_val_3 = result;
    switch (match_val_3.tag) {
        case 0: // Ok
            int value = match_val_3.data.variant_0.arg0;
            if (value != 100) {
                ret_val = 3;
                goto epilogue;
            }
            break;
        case 1: // Err
            ret_val = 4;
            goto epilogue;
            break;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int reference_example(void) {
    int ret_val = 0;
    int x = 10;
    int y = 20;
    int* ref_y = &y;
    int* ref_x = &x;
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int generic_example(void) {
    int ret_val = 0;
    Pair_int int_pair = (Pair_int){.first = 10, .second = 20};
    Option_int opt_int = Option_int_Some_new(42);
    Option_int match_val_4 = opt_int;
    switch (match_val_4.tag) {
        case 0: // Some
            break;
        case 1: // None
            ret_val = 2;
            goto epilogue;
            break;
    }
    Vec_int* vec = ((Vec_int*)(ion_vec_new(sizeof(int))));
    ion_vec_push((ion_vec_t*)(vec), &((int){1}), sizeof(int));
    ion_vec_push((ion_vec_t*)(vec), &((int){2}), sizeof(int));
    int vec_len = ((vec) ? (int)((ion_vec_t*)(vec))->len : 0);
    if (vec_len != 2) {
        ret_val = 3;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int complex_example(void) {
    int ret_val = 0;
    Vec_int* numbers = ((Vec_int*)(ion_vec_new(sizeof(int))));
    ion_vec_push((ion_vec_t*)(numbers), &((int){1}), sizeof(int));
    ion_vec_push((ion_vec_t*)(numbers), &((int){3}), sizeof(int));
    ion_vec_push((ion_vec_t*)(numbers), &((int){5}), sizeof(int));
    int len = ((numbers) ? (int)((ion_vec_t*)(numbers))->len : 0);
    if (len != 3) {
        ret_val = 1;
        goto epilogue;
    }
    Option_int match_val_5 = *((Option_int*)(ion_vec_get((ion_vec_t*)(numbers), 0, sizeof(int))));
    switch (match_val_5.tag) {
        case 0: // Some
            int value = match_val_5.data.variant_0.arg0;
            if (value != 1) {
                ret_val = 2;
                goto epilogue;
            }
            break;
        case 1: // None
            ret_val = 3;
            goto epilogue;
            break;
    }
    Option_int match_val_6 = *((Option_int*)(ion_vec_get((ion_vec_t*)(numbers), 1, sizeof(int))));
    switch (match_val_6.tag) {
        case 0: // Some
            int value = match_val_6.data.variant_0.arg0;
            if (value != 3) {
                ret_val = 4;
                goto epilogue;
            }
            break;
        case 1: // None
            ret_val = 5;
            goto epilogue;
            break;
    }
    ion_string_t* description = ion_string_from_literal("Numbers: ", 9);
    ion_string_push_str(description, "(", 1);
    int desc_len = ((description) ? (int)((ion_vec_t*)(description))->len : 0);
    if (desc_len < 8) {
        ret_val = 6;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int main(void) {
    int ret_val = 0;
    int x = 10;
    int y = 20;
    int z = (x + y);
    if (z != 30) {
        ret_val = 100;
        goto epilogue;
    }
    if (box_example() != 42) {
        ret_val = 1;
        goto epilogue;
    }
    if (vec_example() != 0) {
        ret_val = 2;
        goto epilogue;
    }
    if (string_example() != 0) {
        ret_val = 3;
        goto epilogue;
    }
    if (control_flow_example() != 0) {
        ret_val = 4;
        goto epilogue;
    }
    if (pattern_matching_example() != 0) {
        ret_val = 5;
        goto epilogue;
    }
    if (reference_example() != 0) {
        ret_val = 6;
        goto epilogue;
    }
    if (generic_example() != 0) {
        ret_val = 7;
        goto epilogue;
    }
    if (complex_example() != 0) {
        ret_val = 8;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

