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

int main(void);
int main(void) {
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
    { int _send_val = 42; ion_channel_send(&tx, &_send_val); }
    ion_receiver_t rx_back_mut = rx_back;
    int result = ({ int tmp; ion_channel_recv(&rx_back_mut, &tmp); tmp; });
    if (result == 42) {
        ret_val = 0;
        if (rx_back_mut.channel) { ion_channel_handle_drop(rx_back_mut.channel); }
        if (tx.channel) { ion_channel_handle_drop(tx.channel); }
        goto epilogue;
    }
    ret_val = 1;
    if (rx_back_mut.channel) { ion_channel_handle_drop(rx_back_mut.channel); }
    if (tx.channel) { ion_channel_handle_drop(tx.channel); }
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

