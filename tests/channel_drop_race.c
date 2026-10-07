/* Concurrent sender and receiver drop. The queued element drops once. */
#include "ion_runtime.h"

#include <pthread.h>
#include <stdio.h>

static pthread_barrier_t barrier;
static int dropped;

static void note_drop(void *slot) {
  (void)slot;
  dropped++;
}

static void *drop_tx(void *arg) {
  pthread_barrier_wait(&barrier);
  ion_channel_sender_drop((ion_sender_t *)arg);
  return NULL;
}

static void *drop_rx(void *arg) {
  pthread_barrier_wait(&barrier);
  ion_channel_receiver_drop((ion_receiver_t *)arg);
  return NULL;
}

static int race_pair(void) {
  ion_sender_t tx;
  ion_receiver_t rx;
  int value;
  pthread_t ts;
  pthread_t tr;
  value = 7;
  dropped = 0;
  if (ion_channel_new(sizeof(int), 1, note_drop, &tx, &rx) != 0)
    return 10;
  if (ion_channel_send(&tx, &value) != 0)
    return 11;
  if (pthread_barrier_init(&barrier, NULL, 2) != 0)
    return 12;
  if (pthread_create(&tr, NULL, drop_rx, &rx) != 0)
    return 13;
  if (pthread_create(&ts, NULL, drop_tx, &tx) != 0)
    return 14;
  if (pthread_join(tr, NULL) != 0)
    return 15;
  if (pthread_join(ts, NULL) != 0)
    return 16;
  pthread_barrier_destroy(&barrier);
  if (dropped != 1)
    return 17;
  return 0;
}

static int race_clone(void) {
  ion_sender_t tx;
  ion_sender_t tx2;
  ion_receiver_t rx;
  int value;
  pthread_t t1;
  pthread_t t2;
  pthread_t t3;
  value = 9;
  dropped = 0;
  if (ion_channel_new(sizeof(int), 1, note_drop, &tx, &rx) != 0)
    return 20;
  if (ion_channel_clone_sender(&tx, &tx2) != 0)
    return 21;
  if (ion_channel_send(&tx, &value) != 0)
    return 22;
  if (pthread_barrier_init(&barrier, NULL, 3) != 0)
    return 23;
  if (pthread_create(&t1, NULL, drop_tx, &tx) != 0)
    return 24;
  if (pthread_create(&t2, NULL, drop_tx, &tx2) != 0)
    return 25;
  if (pthread_create(&t3, NULL, drop_rx, &rx) != 0)
    return 26;
  if (pthread_join(t1, NULL) != 0)
    return 27;
  if (pthread_join(t2, NULL) != 0)
    return 28;
  if (pthread_join(t3, NULL) != 0)
    return 29;
  pthread_barrier_destroy(&barrier);
  if (dropped != 1)
    return 30;
  return 0;
}

int main(void) {
  int i;
  int code;
  for (i = 0; i < 200; i++) {
    code = race_pair();
    if (code != 0) {
      fprintf(stderr, "pair %d\n", code);
      return code;
    }
    code = race_clone();
    if (code != 0) {
      fprintf(stderr, "clone %d\n", code);
      return code;
    }
  }
  return 0;
}
