#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "ion_runtime.h"

static void* ion_spawn_entry_0(void* arg);
typedef struct {
    ion_receiver_t rx;
    ion_sender_t tx_back;
} ion_spawn_ctx_0;

typedef struct Vec_int {
    void* data;
    size_t len;
    size_t capacity;
    size_t elem_size;
} Vec_int;

typedef struct tuple_int_int {
    int f0;
    int f1;
} tuple_int_int;

typedef struct tuple_ion_sender_t_ion_receiver_t {
    ion_sender_t f0;
    ion_receiver_t f1;
} tuple_ion_sender_t_ion_receiver_t;

typedef struct Point {
    int x;
    int y;
} Point;

typedef struct Status {
    int tag;
    union {
        struct {
            int value;
        } variant_0;
        struct {
            int code;
        } variant_1;
    } data;
} Status;

static Status Status_Ok_new(int value) {
    Status result = { .tag = 0, .data = { .variant_0 = { .value = value } } };
    return result;
}

static Status Status_Err_new(int code) {
    Status result = { .tag = 1, .data = { .variant_1 = { .code = code } } };
    return result;
}

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

int add(int x, int y);
int multiply(int x, int y);
Point create_point(int x, int y);
int box_example(void);
int vec_example(void);
int for_loop_example(void);
int string_example(void);
int if_example(void);
int while_example(void);
int control_flow_example(void);
int pattern_matching_example(void);
int reference_example(void);
int generic_example(void);
int tuple_example(void);
int spawn_channel_example(void);
int complex_example(void);
int main(void);
int get_first_int(Pair_int pair);
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
        case 0: { // Some
            int value = match_val_0.data.variant_0.arg0;
            if (value != 10) {
                ret_val = 2;
                goto epilogue;
            }
            break;
        }
        case 1: { // None
            ret_val = 3;
            goto epilogue;
            break;
        }
    }
    ion_vec_set((ion_vec_t*)(numbers), 1, &((int){25}), sizeof(int));
    Option_int match_val_1 = *((Option_int*)(ion_vec_pop((ion_vec_t*)(numbers), sizeof(int))));
    switch (match_val_1.tag) {
        case 0: { // Some
            int value = match_val_1.data.variant_0.arg0;
            if (value != 30) {
                ret_val = 4;
                goto epilogue;
            }
            break;
        }
        case 1: { // None
            ret_val = 5;
            goto epilogue;
            break;
        }
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int for_loop_example(void) {
    int ret_val = 0;
    Vec_int* values = ((Vec_int*)(ion_vec_new(sizeof(int))));
    ion_vec_push((ion_vec_t*)(values), &((int){1}), sizeof(int));
    ion_vec_push((ion_vec_t*)(values), &((int){2}), sizeof(int));
    ion_vec_push((ion_vec_t*)(values), &((int){3}), sizeof(int));
    int sum = 0;
    Vec_int* __for_container_2192 = values;
    int __for_i_2192 = 0;
    while (__for_i_2192 < ((__for_container_2192) ? (int)((ion_vec_t*)(__for_container_2192))->len : 0)) {
        Option_int __for_opt_2192 = *((Option_int*)(ion_vec_get((ion_vec_t*)(__for_container_2192), __for_i_2192, sizeof(int))));
        Option_int match_val_2 = __for_opt_2192;
        switch (match_val_2.tag) {
            case 0: { // Some
                int v = match_val_2.data.variant_0.arg0;
                sum = (sum + v);
                break;
            }
            case 1: { // None
                break;
            }
        }
        __for_step_2192:
        __for_i_2192 = (__for_i_2192 + 1);
    }
    if (sum != 6) {
        ret_val = 1;
        if (__for_container_2192) { ion_vec_free((ion_vec_t*)(__for_container_2192)); }
        goto epilogue;
    }
    ret_val = 0;
    if (__for_container_2192) { ion_vec_free((ion_vec_t*)(__for_container_2192)); }
    goto epilogue;
epilogue:
        return ret_val;
}

int string_example(void) {
    int ret_val = 0;
    ion_string_t* greeting = ion_string_new();
    ion_string_push_str(greeting, "Hello", 5);
    ion_string_push_str(greeting, ", ", 2);
    ion_string_push_str(greeting, "Ion", 3);
    ion_string_push_byte(greeting, (unsigned char)((uint8_t)33));
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
    while (x < 3) {
        x = (x + 1);
    }
    if (x != 3) {
        ret_val = 1;
        goto epilogue;
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
    if (for_loop_example() != 0) {
        ret_val = 3;
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
    Option_int match_val_3 = opt;
    switch (match_val_3.tag) {
        case 0: { // Some
            int v = match_val_3.data.variant_0.arg0;
            if ((v == 42)) {
            break;
            }
            ret_val = 1;
            goto epilogue;
            break;
        }
        case 1: { // None
            ret_val = 2;
            goto epilogue;
            break;
        }
    }
    Result_int_int result = Result_int_int_Ok_new(100);
    Result_int_int match_val_4 = result;
    switch (match_val_4.tag) {
        case 0: { // Ok
            int value = match_val_4.data.variant_0.arg0;
            if (value != 100) {
                ret_val = 3;
                goto epilogue;
            }
            break;
        }
        case 1: { // Err
            ret_val = 4;
            goto epilogue;
            break;
        }
    }
    Status status = Status_Ok_new(7);
    Status match_val_5 = status;
    switch (match_val_5.tag) {
        case 0: { // Ok
            int v = match_val_5.data.variant_0.value;
            if (v != 7) {
                ret_val = 5;
                goto epilogue;
            }
            break;
        }
        case 1: { // Err
            int _c = match_val_5.data.variant_1.code;
            ret_val = 6;
            goto epilogue;
            break;
        }
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
    if (1) {
        int* _ref_y = &y;
        int* _ref_x = &x;
    }
    Vec_int* values = ((Vec_int*)(ion_vec_new(sizeof(int))));
    ion_vec_push((ion_vec_t*)(values), &((int){x}), sizeof(int));
    ion_vec_push((ion_vec_t*)(values), &((int){y}), sizeof(int));
    if (((values) ? (int)((ion_vec_t*)(values))->len : 0) != 2) {
        ret_val = 1;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int generic_example(void) {
    int ret_val = 0;
    Pair_int int_pair = (Pair_int){.first = 10, .second = 20};
    if (int_pair.first != 10) {
        ret_val = 1;
        goto epilogue;
    }
    if (get_first_int(int_pair) != 10) {
        ret_val = 2;
        goto epilogue;
    }
    Option_int opt_int = Option_int_Some_new(42);
    Option_int match_val_6 = opt_int;
    switch (match_val_6.tag) {
        case 0: { // Some
            break;
        }
        case 1: { // None
            ret_val = 3;
            goto epilogue;
            break;
        }
    }
    Vec_int* vec = ((Vec_int*)(ion_vec_new(sizeof(int))));
    ion_vec_push((ion_vec_t*)(vec), &((int){1}), sizeof(int));
    ion_vec_push((ion_vec_t*)(vec), &((int){2}), sizeof(int));
    int vec_len = ((vec) ? (int)((ion_vec_t*)(vec))->len : 0);
    if (vec_len != 2) {
        ret_val = 4;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int tuple_example(void) {
    int ret_val = 0;
    tuple_int_int t = (tuple_int_int){.f0 = 3, .f1 = 4};
    if ((t.f0 != 3) || (t.f1 != 4)) {
        ret_val = 1;
        goto epilogue;
    }
    tuple_int_int __ion_tuple_0 = t;
    int a = __ion_tuple_0.f0;
    int b = __ion_tuple_0.f1;
    if ((a != 3) || (b != 4)) {
        ret_val = 2;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int spawn_channel_example(void) {
    int ret_val = 0;
    ion_sender_t tx;
    ion_receiver_t rx;
    ion_channel_new(sizeof(int), 1, &tx, &rx);
    ion_sender_t tx_back;
    ion_receiver_t rx_back;
    ion_channel_new(sizeof(int), 1, &tx_back, &rx_back);
    {
        ion_spawn_ctx_0* ctx = (ion_spawn_ctx_0*)malloc(sizeof(ion_spawn_ctx_0));
        if (!ctx) { ion_panic("spawn allocation failed"); }
        ctx->rx = rx;
        rx = (ion_receiver_t){0};
        ctx->tx_back = tx_back;
        tx_back = (ion_sender_t){0};
        if (ion_spawn(ion_spawn_entry_0, ctx) != 0) { free(ctx); ion_panic("spawn failed"); }
    }
    { int _send_val = 99; ion_channel_send(&tx, &_send_val); }
    ion_receiver_t rx_back_mut = rx_back;
    int result = ({ int tmp; ion_channel_recv(&rx_back_mut, &tmp); tmp; });
    if (result != 99) {
        ret_val = 1;
        if (rx_back_mut.channel) { ion_channel_handle_drop(rx_back_mut.channel); }
        if (tx.channel) { ion_channel_handle_drop(tx.channel); }
        goto epilogue;
    }
    ret_val = 0;
    if (rx_back_mut.channel) { ion_channel_handle_drop(rx_back_mut.channel); }
    if (tx.channel) { ion_channel_handle_drop(tx.channel); }
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
    Option_int match_val_7 = *((Option_int*)(ion_vec_get((ion_vec_t*)(numbers), 0, sizeof(int))));
    switch (match_val_7.tag) {
        case 0: { // Some
            int value = match_val_7.data.variant_0.arg0;
            if (value != 1) {
                ret_val = 2;
                goto epilogue;
            }
            break;
        }
        case 1: { // None
            ret_val = 3;
            goto epilogue;
            break;
        }
    }
    Option_int match_val_8 = *((Option_int*)(ion_vec_get((ion_vec_t*)(numbers), 1, sizeof(int))));
    switch (match_val_8.tag) {
        case 0: { // Some
            int value = match_val_8.data.variant_0.arg0;
            if (value != 3) {
                ret_val = 4;
                goto epilogue;
            }
            break;
        }
        case 1: { // None
            ret_val = 5;
            goto epilogue;
            break;
        }
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
    if (tuple_example() != 0) {
        ret_val = 8;
        goto epilogue;
    }
    if (spawn_channel_example() != 0) {
        ret_val = 9;
        goto epilogue;
    }
    if (complex_example() != 0) {
        ret_val = 10;
        goto epilogue;
    }
    ret_val = 0;
    goto epilogue;
epilogue:
        return ret_val;
}

int get_first_int(Pair_int pair) {
    int ret_val = 0;
    ret_val = pair.first;
    goto epilogue;
epilogue:
        return ret_val;
}


static void* ion_spawn_entry_0(void* arg) {
    ion_spawn_ctx_0* ctx = (ion_spawn_ctx_0*)arg;
    if (!ctx) { ion_panic("spawn null context"); }
    ion_receiver_t rx = ctx->rx;
    ion_sender_t tx_back = ctx->tx_back;
    free(ctx);
    ion_receiver_t rx_mut = rx;
    int value = ({ int tmp; ion_channel_recv(&rx_mut, &tmp); tmp; });
    { ion_channel_send(&tx_back, &value); }
    if (rx_mut.channel) { ion_channel_handle_drop(rx_mut.channel); }
    if (tx_back.channel) { ion_channel_handle_drop(tx_back.channel); }
    goto spawn_0_epilogue;
spawn_0_epilogue:
    return NULL;
}

