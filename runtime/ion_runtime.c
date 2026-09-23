#include "ion_runtime.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <stdatomic.h>
#ifdef _WIN32
#include <winsock2.h>
#ifdef _MSC_VER
#pragma comment(lib, "ws2_32.lib")
#endif
#endif

#if defined(_WIN32) && !defined(CLOCK_REALTIME)
#include <sys/time.h>
static int ion_clock_gettime_realtime(struct timespec *ts) {
  struct timeval tv;
  if (gettimeofday(&tv, NULL) != 0)
    return -1;
  ts->tv_sec = tv.tv_sec;
  ts->tv_nsec = tv.tv_usec * 1000L;
  return 0;
}
#define ion_clock_gettime(ts) ion_clock_gettime_realtime(ts)
#else
#define ion_clock_gettime(ts) clock_gettime(CLOCK_REALTIME, (ts))
#endif

// ============================================================================
// Safety and Error Handling
// ============================================================================

void ion_panic(const char *message) {
  fprintf(stderr, "Ion panic: %s\n", message != NULL ? message : "");
  abort();
}

void ion_abort_bytes(uint8_t *message) {
  ion_panic(message != NULL ? (const char *)message : "");
}

// ============================================================================
// Vec Implementation
// ============================================================================

ion_vec_t *ion_vec_new(size_t elem_size) {
  ion_vec_t *vec = (ion_vec_t *)malloc(sizeof(ion_vec_t));
  if (!vec)
    return NULL;

  vec->data = NULL;
  vec->len = 0;
  vec->capacity = 0;
  vec->elem_size = elem_size;

  return vec;
}

ion_vec_t *ion_vec_with_capacity(size_t elem_size, int capacity) {
  ion_vec_t *vec = ion_vec_new(elem_size);
  if (!vec)
    return NULL;

  if (capacity > 0) {
    vec->data = malloc(elem_size * capacity);
    if (!vec->data) {
      free(vec);
      return NULL;
    }
    vec->capacity = capacity;
  }

  return vec;
}

int ion_vec_set(ion_vec_t *vec, int index, const void *value,
                size_t elem_size) {
  if (!vec || !value || index < 0 || (size_t)index >= vec->len) {
    return -1;
  }
  if (elem_size != vec->elem_size)
    return -1;

  memcpy((char *)vec->data + (index * elem_size), value, elem_size);
  return 0;
}

void ion_vec_free(ion_vec_t *vec) {
  if (!vec)
    return;
  if (vec->data)
    free(vec->data);
  free(vec);
}

// ============================================================================
// String Implementation
// ============================================================================

ion_string_t *ion_string_new(void) {
  ion_string_t *s = (ion_string_t *)malloc(sizeof(ion_string_t));
  if (!s)
    return NULL;

  s->data = (uint8_t *)malloc(1);
  if (!s->data) {
    free(s);
    return NULL;
  }
  s->data[0] = '\0';
  s->len = 0;
  s->capacity = 1;

  return s;
}

int ion_utf8_valid(const uint8_t *data, size_t len) {
  size_t i = 0;
  if (len == 0)
    return 1;
  if (!data)
    return 0;
  while (i < len) {
    uint8_t b = data[i];
    size_t need;
    uint32_t cp;
    size_t j;
    if (b <= 0x7F) {
      i++;
      continue;
    }
    if ((b & 0xE0) == 0xC0) {
      need = 1;
      cp = (uint32_t)(b & 0x1F);
      if (b < 0xC2)
        return 0;
    } else if ((b & 0xF0) == 0xE0) {
      need = 2;
      cp = (uint32_t)(b & 0x0F);
    } else if ((b & 0xF8) == 0xF0) {
      need = 3;
      cp = (uint32_t)(b & 0x07);
      if (b > 0xF4)
        return 0;
    } else {
      return 0;
    }
    if (i + 1 + need > len)
      return 0;
    for (j = 0; j < need; j++) {
      uint8_t c = data[i + 1 + j];
      if ((c & 0xC0) != 0x80)
        return 0;
      cp = (cp << 6) | (uint32_t)(c & 0x3F);
    }
    if (need == 2 && cp < 0x800)
      return 0;
    if (need == 3 && cp < 0x10000)
      return 0;
    if (cp > 0x10FFFF)
      return 0;
    if (cp >= 0xD800 && cp <= 0xDFFF)
      return 0;
    i += 1 + need;
  }
  return 1;
}

ion_string_t *ion_string_from_literal(const char *lit, size_t len) {
  ion_string_t *s;
  if (!ion_utf8_valid((const uint8_t *)lit, len))
    return NULL;

  s = (ion_string_t *)malloc(sizeof(ion_string_t));
  if (!s)
    return NULL;

  s->data = (uint8_t *)malloc(len + 1);
  if (!s->data) {
    free(s);
    return NULL;
  }
  if (len > 0 && lit)
    memcpy(s->data, lit, len);
  s->data[len] = '\0';
  s->len = len;
  s->capacity = len + 1;

  return s;
}

ion_string_t *ion_string_clone(const ion_string_t *s) {
  if (!s)
    return NULL;
  return ion_string_from_literal((const char *)s->data, s->len);
}

int ion_string_push_str(ion_string_t *s, const char *other, size_t other_len) {
  if (!s || !other)
    return -1;

  // If other_len is 0, treat other as a string pointer and get its length
  size_t append_len = other_len;
  if (append_len == 0) {
    append_len = strlen(other);
  }

  if (!ion_utf8_valid((const uint8_t *)other, append_len))
    return -1;

  if (s->len + append_len + 1 > s->capacity) {
    size_t new_capacity = s->capacity;
    while (new_capacity < s->len + append_len + 1) {
      new_capacity *= 2;
    }
    uint8_t *new_data = (uint8_t *)realloc(s->data, new_capacity);
    if (!new_data)
      return -1;
    s->data = new_data;
    s->capacity = new_capacity;
  }

  memcpy(s->data + s->len, other, append_len);
  s->len += append_len;
  s->data[s->len] = '\0';

  return 0;
}

int ion_string_push_byte(ion_string_t *s, unsigned char b) {
  if (!s)
    return -1;
  if (b >= 0x80)
    return -2;

  if (s->len + 2 > s->capacity) {
    size_t new_capacity = s->capacity;
    while (new_capacity < s->len + 2) {
      new_capacity *= 2;
    }
    uint8_t *new_data = (uint8_t *)realloc(s->data, new_capacity);
    if (!new_data)
      return -1;
    s->data = new_data;
    s->capacity = new_capacity;
  }

  s->data[s->len] = (char)b;
  s->len += 1;
  s->data[s->len] = '\0';

  return 0;
}

int ion_string_equals(const ion_string_t *a, const ion_string_t *b) {
  if (a == b)
    return 1;
  if (!a || !b)
    return 0;
  if (a->len != b->len)
    return 0;
  return memcmp(a->data, b->data, a->len) == 0;
}

void ion_string_free(ion_string_t *s) {
  if (!s)
    return;
  if (s->data)
    free(s->data);
  free(s);
}

// ============================================================================
// Networking (platform socket setup)
// ============================================================================

void ion_net_init(void) {
#ifdef _WIN32
  WSADATA wsa;
  if (WSAStartup(MAKEWORD(2, 2), &wsa) != 0)
    ion_panic("WSAStartup failed");
#endif
}

// ============================================================================
// Threading
// ============================================================================

typedef char ion_thread_storage_ok[(sizeof(pthread_t) <= 16) ? 1 : -1];

int ion_spawn(void *(*start_routine)(void *), void *arg) {
  pthread_t thread;
  int rc = pthread_create(&thread, NULL, start_routine, arg);
  if (rc != 0)
    return rc;
  pthread_detach(thread);
  return 0;
}

static void ion_thread_store(ion_thread_t *out, pthread_t thread) {
  memset(out->thread, 0, sizeof(out->thread));
  memcpy(out->thread, &thread, sizeof(pthread_t));
  out->live = 1;
}

static pthread_t ion_thread_load(const ion_thread_t *t) {
  pthread_t thread;
  memcpy(&thread, t->thread, sizeof(pthread_t));
  return thread;
}

int ion_spawn_joinable(void *(*start_routine)(void *), void *arg,
                       ion_thread_t *out) {
  pthread_t thread;
  int rc;
  if (!out)
    return -1;
  rc = pthread_create(&thread, NULL, start_routine, arg);
  if (rc != 0)
    return rc;
  ion_thread_store(out, thread);
  return 0;
}

int ion_join(ion_thread_t *thread) {
  int rc;
  if (!thread || !thread->live)
    return -1;
  rc = pthread_join(ion_thread_load(thread), NULL);
  thread->live = 0;
  memset(thread->thread, 0, sizeof(thread->thread));
  return rc;
}

void ion_thread_detach(ion_thread_t *thread) {
  if (!thread || !thread->live)
    return;
  pthread_detach(ion_thread_load(thread));
  thread->live = 0;
  memset(thread->thread, 0, sizeof(thread->thread));
}

// ============================================================================
// Channel Implementation
// ============================================================================

struct ion_select_waiter {
  pthread_mutex_t *mu;
  pthread_cond_t *cv;
  int *ready;
  struct ion_select_waiter *next;
};

/* One receiver. Several senders share the channel via clone_sender.
 * The success path claims a slot with atomics and copies elem_size bytes.
 * A thread parks on wait_mu only when the buffer is full, empty, or
 * disconnected, and only after its waiter count is published and the claim
 * is retried. A successful operation locks and signals only when that count
 * is non-zero. Last sender drop disconnects receive and wakes receivers.
 * Last receiver drop disconnects send and wakes senders. Counts are separate.
 * Destroy runs only when both are 0, after copies have finished, and drops
 * each buffered element once. Capacity 1 uses the same rules. */
struct ion_channel_t {
  atomic_size_t head;
  char pad_head[64];
  atomic_size_t tail;
  char pad_tail[64];
  void *data;
  atomic_size_t *stamps;
  size_t elem_size;
  size_t cap;
  size_t mark_bit;
  size_t one_lap;
  atomic_int sender_count;
  atomic_int receiver_count;
  atomic_int destroy_once;
  atomic_int senders_waiting;
  atomic_int receivers_waiting;
  atomic_int select_waiting;
  void (*drop_fn)(void *);
  pthread_mutex_t wait_mu;
  pthread_cond_t senders_cv;
  pthread_cond_t receivers_cv;
  struct ion_select_waiter *waiters;
};

enum {
  ION_SLOT_OK = 0,
  ION_SLOT_FULL = 1,
  ION_SLOT_CLOSED = 2,
  ION_SLOT_EMPTY = 3
};

struct ion_slot_token {
  size_t index;
  size_t stamp;
};

static void ion_spin_pause(void) {
#if defined(__i386__) || defined(__x86_64__)
  __asm__ __volatile__("pause");
#elif defined(__aarch64__)
  __asm__ __volatile__("yield");
#else
  atomic_signal_fence(memory_order_seq_cst);
#endif
}

static size_t ion_next_pow2_size(size_t x) {
  size_t p = 1;
  if (x == 0)
    return 1;
  while (p < x) {
    if (p > (SIZE_MAX >> 1))
      ion_panic("channel capacity too large");
    p <<= 1;
  }
  return p;
}

static void *ion_channel_slot_data(struct ion_channel_t *ch, size_t index) {
  return (char *)ch->data + (index * ch->elem_size);
}

static int ion_channel_is_disconnected(struct ion_channel_t *ch) {
  size_t tail = atomic_load_explicit(&ch->tail, memory_order_seq_cst);
  return (tail & ch->mark_bit) != 0;
}

static int ion_channel_is_empty(struct ion_channel_t *ch) {
  size_t head = atomic_load_explicit(&ch->head, memory_order_seq_cst);
  size_t tail = atomic_load_explicit(&ch->tail, memory_order_seq_cst);
  return (tail & ~ch->mark_bit) == head;
}

static int ion_channel_is_full(struct ion_channel_t *ch) {
  size_t tail = atomic_load_explicit(&ch->tail, memory_order_seq_cst);
  size_t head = atomic_load_explicit(&ch->head, memory_order_seq_cst);
  return (head + ch->one_lap) == (tail & ~ch->mark_bit);
}

static void ion_channel_wake_select_locked(struct ion_channel_t *ch) {
  struct ion_select_waiter *w;
  for (w = ch->waiters; w; w = w->next) {
    pthread_mutex_lock(w->mu);
    *w->ready = 1;
    pthread_cond_signal(w->cv);
    pthread_mutex_unlock(w->mu);
  }
}

static void ion_channel_notify_receivers(struct ion_channel_t *ch) {
  if (atomic_load_explicit(&ch->receivers_waiting, memory_order_acquire) == 0 &&
      atomic_load_explicit(&ch->select_waiting, memory_order_acquire) == 0)
    return;
  pthread_mutex_lock(&ch->wait_mu);
  if (atomic_load_explicit(&ch->receivers_waiting, memory_order_relaxed) != 0)
    pthread_cond_broadcast(&ch->receivers_cv);
  if (atomic_load_explicit(&ch->select_waiting, memory_order_relaxed) != 0)
    ion_channel_wake_select_locked(ch);
  pthread_mutex_unlock(&ch->wait_mu);
}

static void ion_channel_notify_senders(struct ion_channel_t *ch) {
  if (atomic_load_explicit(&ch->senders_waiting, memory_order_acquire) == 0)
    return;
  pthread_mutex_lock(&ch->wait_mu);
  if (atomic_load_explicit(&ch->senders_waiting, memory_order_relaxed) != 0)
    pthread_cond_broadcast(&ch->senders_cv);
  pthread_mutex_unlock(&ch->wait_mu);
}

/* Walk published slots once. Called only when both handle counts are 0, so no
 * send or recv is still copying. */
static void ion_channel_discard_buffered(struct ion_channel_t *ch) {
  size_t head;
  size_t tail;
  if (!ch->data || !ch->stamps)
    return;
  head = atomic_load_explicit(&ch->head, memory_order_relaxed);
  tail = atomic_load_explicit(&ch->tail, memory_order_seq_cst) & ~ch->mark_bit;
  for (;;) {
    size_t index = head & (ch->mark_bit - 1);
    size_t lap = head & ~(ch->one_lap - 1);
    size_t stamp;
    if (index >= ch->cap)
      ion_panic("channel slot index out of range");
    stamp = atomic_load_explicit(&ch->stamps[index], memory_order_acquire);
    if (head + 1 == stamp) {
      if (ch->drop_fn)
        ch->drop_fn(ion_channel_slot_data(ch, index));
      if (index + 1 < ch->cap)
        head = head + 1;
      else
        head = lap + ch->one_lap;
    } else if (tail == head) {
      return;
    } else {
      ion_spin_pause();
    }
  }
}

static void ion_channel_destroy(struct ion_channel_t *ch) {
  if (!ch)
    return;
  ion_channel_discard_buffered(ch);
  free(ch->data);
  free(ch->stamps);
  pthread_cond_destroy(&ch->senders_cv);
  pthread_cond_destroy(&ch->receivers_cv);
  pthread_mutex_destroy(&ch->wait_mu);
  free(ch);
}

static int ion_channel_start_send(struct ion_channel_t *ch,
                                  struct ion_slot_token *token) {
  size_t tail = atomic_load_explicit(&ch->tail, memory_order_relaxed);
  for (;;) {
    size_t index;
    size_t lap;
    size_t stamp;
    if (tail & ch->mark_bit) {
      token->index = 0;
      token->stamp = 0;
      return ION_SLOT_CLOSED;
    }
    index = tail & (ch->mark_bit - 1);
    lap = tail & ~(ch->one_lap - 1);
    if (index >= ch->cap)
      ion_panic("channel slot index out of range");
    stamp = atomic_load_explicit(&ch->stamps[index], memory_order_acquire);
    if (tail == stamp) {
      size_t new_tail = (index + 1 < ch->cap) ? (tail + 1) : (lap + ch->one_lap);
      size_t expected = tail;
      if (atomic_compare_exchange_weak_explicit(&ch->tail, &expected, new_tail,
                                                memory_order_seq_cst,
                                                memory_order_relaxed)) {
        token->index = index;
        token->stamp = tail + 1;
        return ION_SLOT_OK;
      }
      tail = expected;
      ion_spin_pause();
    } else if (stamp + ch->one_lap == tail + 1) {
      size_t head;
      atomic_thread_fence(memory_order_seq_cst);
      head = atomic_load_explicit(&ch->head, memory_order_relaxed);
      if (head + ch->one_lap == tail)
        return ION_SLOT_FULL;
      ion_spin_pause();
      tail = atomic_load_explicit(&ch->tail, memory_order_relaxed);
    } else {
      ion_spin_pause();
      tail = atomic_load_explicit(&ch->tail, memory_order_relaxed);
    }
  }
}

static int ion_channel_start_recv(struct ion_channel_t *ch,
                                  struct ion_slot_token *token) {
  size_t head = atomic_load_explicit(&ch->head, memory_order_relaxed);
  for (;;) {
    size_t index;
    size_t lap;
    size_t stamp;
    index = head & (ch->mark_bit - 1);
    lap = head & ~(ch->one_lap - 1);
    if (index >= ch->cap)
      ion_panic("channel slot index out of range");
    stamp = atomic_load_explicit(&ch->stamps[index], memory_order_acquire);
    if (head + 1 == stamp) {
      size_t new_head = (index + 1 < ch->cap) ? (head + 1) : (lap + ch->one_lap);
      size_t expected = head;
      if (atomic_compare_exchange_weak_explicit(&ch->head, &expected, new_head,
                                                memory_order_seq_cst,
                                                memory_order_relaxed)) {
        token->index = index;
        token->stamp = head + ch->one_lap;
        return ION_SLOT_OK;
      }
      head = expected;
      ion_spin_pause();
    } else if (stamp == head) {
      size_t tail;
      atomic_thread_fence(memory_order_seq_cst);
      tail = atomic_load_explicit(&ch->tail, memory_order_relaxed);
      if ((tail & ~ch->mark_bit) == head) {
        if (tail & ch->mark_bit) {
          token->index = 0;
          token->stamp = 0;
          return ION_SLOT_CLOSED;
        }
        return ION_SLOT_EMPTY;
      }
      ion_spin_pause();
      head = atomic_load_explicit(&ch->head, memory_order_relaxed);
    } else {
      ion_spin_pause();
      head = atomic_load_explicit(&ch->head, memory_order_relaxed);
    }
  }
}

static void ion_channel_publish(struct ion_channel_t *ch,
                                const struct ion_slot_token *token) {
  atomic_store_explicit(&ch->stamps[token->index], token->stamp,
                        memory_order_release);
}

static void ion_channel_write_slot(struct ion_channel_t *ch,
                                   const struct ion_slot_token *token,
                                   const void *value) {
  if (ch->elem_size != 0)
    memcpy(ion_channel_slot_data(ch, token->index), value, ch->elem_size);
  ion_channel_publish(ch, token);
}

static void ion_channel_read_slot(struct ion_channel_t *ch,
                                  const struct ion_slot_token *token,
                                  void *out_value) {
  if (ch->elem_size != 0)
    memcpy(out_value, ion_channel_slot_data(ch, token->index), ch->elem_size);
  ion_channel_publish(ch, token);
}

int ion_channel_new(size_t elem_size, int capacity, void (*drop_fn)(void *),
                    ion_sender_t *sender_out, ion_receiver_t *receiver_out) {
  struct ion_channel_t *ch;
  size_t cap;
  size_t i;
  size_t mark_bit;
  if (!sender_out || !receiver_out)
    return -1;
  if (capacity < 1)
    ion_panic("channel capacity must be >= 1");

  cap = (size_t)capacity;
  mark_bit = ion_next_pow2_size(cap + 1);
  if (mark_bit > SIZE_MAX / 2)
    ion_panic("channel capacity too large");
  if (elem_size != 0 && cap > SIZE_MAX / elem_size)
    ion_panic("channel capacity too large");

  ch = (struct ion_channel_t *)malloc(sizeof(struct ion_channel_t));
  if (!ch)
    return -1;
  memset(ch, 0, sizeof(*ch));
  ch->elem_size = elem_size;
  ch->cap = cap;
  ch->mark_bit = mark_bit;
  ch->one_lap = mark_bit * 2;
  ch->drop_fn = drop_fn;
  atomic_init(&ch->head, 0);
  atomic_init(&ch->tail, 0);
  atomic_init(&ch->sender_count, 1);
  atomic_init(&ch->receiver_count, 1);
  atomic_init(&ch->destroy_once, 0);
  atomic_init(&ch->senders_waiting, 0);
  atomic_init(&ch->receivers_waiting, 0);
  atomic_init(&ch->select_waiting, 0);

  ch->stamps = (atomic_size_t *)calloc(cap, sizeof(atomic_size_t));
  if (!ch->stamps) {
    free(ch);
    return -1;
  }
  for (i = 0; i < cap; i++)
    atomic_init(&ch->stamps[i], i);

  if (elem_size == 0) {
    ch->data = NULL;
  } else {
    ch->data = calloc(cap, elem_size);
    if (!ch->data) {
      free(ch->stamps);
      free(ch);
      return -1;
    }
  }

  if (pthread_mutex_init(&ch->wait_mu, NULL) != 0) {
    free(ch->data);
    free(ch->stamps);
    free(ch);
    return -1;
  }
  if (pthread_cond_init(&ch->senders_cv, NULL) != 0) {
    pthread_mutex_destroy(&ch->wait_mu);
    free(ch->data);
    free(ch->stamps);
    free(ch);
    return -1;
  }
  if (pthread_cond_init(&ch->receivers_cv, NULL) != 0) {
    pthread_cond_destroy(&ch->senders_cv);
    pthread_mutex_destroy(&ch->wait_mu);
    free(ch->data);
    free(ch->stamps);
    free(ch);
    return -1;
  }

  sender_out->channel = (ion_channel_t *)ch;
  sender_out->elem_size = elem_size;
  receiver_out->channel = (ion_channel_t *)ch;
  receiver_out->elem_size = elem_size;
  return 0;
}

static void ion_channel_close_tail(struct ion_channel_t *ch) {
  atomic_fetch_or_explicit(&ch->tail, ch->mark_bit, memory_order_seq_cst);
}

static void ion_channel_maybe_destroy(struct ion_channel_t *ch) {
  int senders = atomic_load_explicit(&ch->sender_count, memory_order_acquire);
  int receivers = atomic_load_explicit(&ch->receiver_count, memory_order_acquire);
  if (senders <= 0 && receivers <= 0 &&
      atomic_exchange_explicit(&ch->destroy_once, 1, memory_order_acq_rel) == 0)
    ion_channel_destroy(ch);
}

static void ion_channel_wait_senders(struct ion_channel_t *ch) {
  pthread_mutex_lock(&ch->wait_mu);
  atomic_fetch_add_explicit(&ch->senders_waiting, 1, memory_order_release);
  while (ion_channel_is_full(ch) && !ion_channel_is_disconnected(ch))
    pthread_cond_wait(&ch->senders_cv, &ch->wait_mu);
  atomic_fetch_sub_explicit(&ch->senders_waiting, 1, memory_order_acq_rel);
  pthread_mutex_unlock(&ch->wait_mu);
}

static void ion_channel_wait_receivers(struct ion_channel_t *ch) {
  pthread_mutex_lock(&ch->wait_mu);
  atomic_fetch_add_explicit(&ch->receivers_waiting, 1, memory_order_release);
  while (ion_channel_is_empty(ch) && !ion_channel_is_disconnected(ch))
    pthread_cond_wait(&ch->receivers_cv, &ch->wait_mu);
  atomic_fetch_sub_explicit(&ch->receivers_waiting, 1, memory_order_acq_rel);
  pthread_mutex_unlock(&ch->wait_mu);
}

int ion_channel_send(const ion_sender_t *sender, const void *value) {
  struct ion_channel_t *ch;
  struct ion_slot_token token;
  int st;
  if (!sender || !sender->channel || !value)
    return -1;
  ch = (struct ion_channel_t *)sender->channel;
  for (;;) {
    st = ion_channel_start_send(ch, &token);
    if (st == ION_SLOT_CLOSED)
      return -1;
    if (st == ION_SLOT_OK) {
      ion_channel_write_slot(ch, &token, value);
      ion_channel_notify_receivers(ch);
      return 0;
    }
    ion_channel_wait_senders(ch);
  }
}

int ion_channel_recv(ion_receiver_t *receiver, void *out_value) {
  struct ion_channel_t *ch;
  struct ion_slot_token token;
  int st;
  if (!receiver || !receiver->channel || !out_value)
    return -1;
  ch = (struct ion_channel_t *)receiver->channel;
  for (;;) {
    st = ion_channel_start_recv(ch, &token);
    if (st == ION_SLOT_CLOSED)
      return -1;
    if (st == ION_SLOT_OK) {
      ion_channel_read_slot(ch, &token, out_value);
      ion_channel_notify_senders(ch);
      return 0;
    }
    ion_channel_wait_receivers(ch);
  }
}

int ion_channel_clone_sender(const ion_sender_t *src, ion_sender_t *dst) {
  struct ion_channel_t *ch;
  if (!src || !src->channel || !dst)
    return -1;
  ch = (struct ion_channel_t *)src->channel;
  atomic_fetch_add_explicit(&ch->sender_count, 1, memory_order_acq_rel);
  dst->channel = src->channel;
  dst->elem_size = src->elem_size;
  return 0;
}

void ion_channel_sender_drop(ion_sender_t *sender) {
  struct ion_channel_t *ch;
  int left;
  if (!sender || !sender->channel)
    return;
  ch = (struct ion_channel_t *)sender->channel;
  sender->channel = NULL;
  left = atomic_fetch_sub_explicit(&ch->sender_count, 1, memory_order_acq_rel) - 1;
  if (left <= 0) {
    ion_channel_close_tail(ch);
    ion_channel_notify_receivers(ch);
  }
  ion_channel_maybe_destroy(ch);
}

void ion_channel_receiver_drop(ion_receiver_t *receiver) {
  struct ion_channel_t *ch;
  int left;
  if (!receiver || !receiver->channel)
    return;
  ch = (struct ion_channel_t *)receiver->channel;
  receiver->channel = NULL;
  left = atomic_fetch_sub_explicit(&ch->receiver_count, 1, memory_order_acq_rel) - 1;
  if (left <= 0) {
    ion_channel_close_tail(ch);
    ion_channel_notify_senders(ch);
    ion_channel_notify_receivers(ch);
  }
  ion_channel_maybe_destroy(ch);
}

int ion_channel_try_send(const ion_sender_t *sender, const void *value) {
  struct ion_channel_t *ch;
  struct ion_slot_token token;
  int st;
  if (!sender || !sender->channel || !value)
    return -1;
  ch = (struct ion_channel_t *)sender->channel;
  st = ion_channel_start_send(ch, &token);
  if (st == ION_SLOT_CLOSED)
    return -1;
  if (st == ION_SLOT_FULL)
    return -2;
  ion_channel_write_slot(ch, &token, value);
  ion_channel_notify_receivers(ch);
  return 0;
}

int ion_channel_try_recv(ion_receiver_t *receiver, void *out_value) {
  struct ion_channel_t *ch;
  struct ion_slot_token token;
  int st;
  if (!receiver || !receiver->channel || !out_value)
    return -1;
  ch = (struct ion_channel_t *)receiver->channel;
  st = ion_channel_start_recv(ch, &token);
  if (st == ION_SLOT_CLOSED)
    return -1;
  if (st == ION_SLOT_EMPTY)
    return -2;
  ion_channel_read_slot(ch, &token, out_value);
  ion_channel_notify_senders(ch);
  return 0;
}

static void ion_select_deadline(struct timespec *ts, int timeout_ms) {
  if (ion_clock_gettime(ts) != 0)
    ion_panic("select clock failed");
  ts->tv_sec += timeout_ms / 1000;
  ts->tv_nsec += (long)(timeout_ms % 1000) * 1000000L;
  if (ts->tv_nsec >= 1000000000L) {
    ts->tv_sec += 1;
    ts->tv_nsec -= 1000000000L;
  }
}

static int ion_select_try_arms(ion_select_arm_t *arms, int n, int *status_out) {
  int i;
  for (i = 0; i < n; i++) {
    int st;
    if (!arms[i].rx || !arms[i].out)
      continue;
    st = ion_channel_try_recv(arms[i].rx, arms[i].out);
    if (st == 0 || st == -1) {
      if (status_out)
        *status_out = st;
      return i;
    }
  }
  return -1;
}

static void ion_select_register(ion_select_arm_t *arms, int n,
                                struct ion_select_waiter *waiter) {
  int i;
  for (i = 0; i < n; i++) {
    struct ion_channel_t *ch;
    if (!arms[i].rx || !arms[i].rx->channel)
      continue;
    ch = (struct ion_channel_t *)arms[i].rx->channel;
    pthread_mutex_lock(&ch->wait_mu);
    waiter[i].next = ch->waiters;
    ch->waiters = &waiter[i];
    atomic_fetch_add_explicit(&ch->select_waiting, 1, memory_order_release);
    pthread_mutex_unlock(&ch->wait_mu);
  }
}

static void ion_select_unregister(ion_select_arm_t *arms, int n,
                                  struct ion_select_waiter *waiter) {
  int i;
  for (i = 0; i < n; i++) {
    struct ion_channel_t *ch;
    struct ion_select_waiter **slot;
    if (!arms[i].rx || !arms[i].rx->channel)
      continue;
    ch = (struct ion_channel_t *)arms[i].rx->channel;
    pthread_mutex_lock(&ch->wait_mu);
    slot = &ch->waiters;
    while (*slot) {
      if (*slot == &waiter[i]) {
        *slot = waiter[i].next;
        atomic_fetch_sub_explicit(&ch->select_waiting, 1, memory_order_acq_rel);
        break;
      }
      slot = &(*slot)->next;
    }
    waiter[i].next = NULL;
    pthread_mutex_unlock(&ch->wait_mu);
  }
}

static void ion_select_finish(pthread_mutex_t *mu, pthread_cond_t *cv,
                              struct ion_select_waiter *waiters) {
  free(waiters);
  pthread_cond_destroy(cv);
  pthread_mutex_destroy(mu);
}

int ion_channel_select(ion_select_arm_t *arms, int n, int timeout_ms,
                       int *status_out) {
  pthread_mutex_t mu;
  pthread_cond_t cv;
  int ready = 0;
  struct ion_select_waiter *waiters;
  struct timespec deadline;
  int has_deadline;
  int i;
  int idx;
  int registered;

  if (timeout_ms < -1)
    ion_panic("select timeout must be >= 0");
  if (n < 0)
    return -1;
  if (n > 0 && !arms)
    return -1;

  has_deadline = timeout_ms > 0;
  if (has_deadline)
    ion_select_deadline(&deadline, timeout_ms);

  if (pthread_mutex_init(&mu, NULL) != 0)
    ion_panic("select mutex init failed");
  if (pthread_cond_init(&cv, NULL) != 0) {
    pthread_mutex_destroy(&mu);
    ion_panic("select cond init failed");
  }

  waiters = NULL;
  if (n > 0) {
    waiters = (struct ion_select_waiter *)calloc((size_t)n,
                                                 sizeof(struct ion_select_waiter));
    if (!waiters)
      ion_panic("select allocation failed");
    for (i = 0; i < n; i++) {
      waiters[i].mu = &mu;
      waiters[i].cv = &cv;
      waiters[i].ready = &ready;
      waiters[i].next = NULL;
    }
  }

  idx = ion_select_try_arms(arms, n, status_out);
  if (idx >= 0) {
    ion_select_finish(&mu, &cv, waiters);
    return idx;
  }
  if (timeout_ms == 0) {
    ion_select_finish(&mu, &cv, waiters);
    return n;
  }

  /* Register before the empty recheck. A send that lands before the waiter
   * is registered is taken by try_recv. A send after registration sets the
   * sticky ready flag. */
  ready = 0;
  registered = 0;
  if (n > 0) {
    ion_select_register(arms, n, waiters);
    registered = 1;
  }

  for (;;) {
    idx = ion_select_try_arms(arms, n, status_out);
    if (idx >= 0) {
      if (registered)
        ion_select_unregister(arms, n, waiters);
      ion_select_finish(&mu, &cv, waiters);
      return idx;
    }

    pthread_mutex_lock(&mu);
    while (!ready) {
      if (has_deadline) {
        int rc = pthread_cond_timedwait(&cv, &mu, &deadline);
        if (rc == ETIMEDOUT) {
          pthread_mutex_unlock(&mu);
          if (registered)
            ion_select_unregister(arms, n, waiters);
          idx = ion_select_try_arms(arms, n, status_out);
          ion_select_finish(&mu, &cv, waiters);
          if (idx >= 0)
            return idx;
          return n;
        }
      } else {
        pthread_cond_wait(&cv, &mu);
      }
    }
    ready = 0;
    pthread_mutex_unlock(&mu);
  }
}

ion_file_t ion_file_open(const char *path, const char *mode) {
  ion_file_t file;
  file.fp = NULL;
  if (!path || !mode)
    return file;
  file.fp = fopen(path, mode);
  return file;
}

int ion_file_read(ion_file_t *file, void *buf, size_t n, size_t *out_n) {
  size_t got;
  if (!file || !file->fp || !buf || !out_n)
    return -1;
  got = fread(buf, 1, n, (FILE *)file->fp);
  *out_n = got;
  if (got < n && ferror((FILE *)file->fp))
    return -1;
  return 0;
}

int ion_file_write(ion_file_t *file, const void *buf, size_t n, size_t *out_n) {
  size_t put;
  if (!file || !file->fp || (!buf && n > 0) || !out_n)
    return -1;
  put = fwrite(buf, 1, n, (FILE *)file->fp);
  *out_n = put;
  if (put < n)
    return -1;
  return 0;
}

void ion_file_close(ion_file_t *file) {
  if (!file || !file->fp)
    return;
  fclose((FILE *)file->fp);
  file->fp = NULL;
}

int ion_millis(void) {
  struct timespec ts;
  uint64_t ms;
  if (ion_clock_gettime(&ts) != 0)
    return 0;
  ms = (uint64_t)ts.tv_sec * 1000u + (uint64_t)ts.tv_nsec / 1000000u;
  return (int)(ms & 0x7fffffff);
}

int ion_env_copy(uint8_t *name, uint8_t *buf, int cap) {
  const char *value;
  int n;
  if (!name || !buf || cap <= 0)
    return -1;
  value = getenv((const char *)name);
  if (!value)
    return -1;
  n = (int)strlen(value);
  if (n >= cap)
    n = cap - 1;
  if (n > 0)
    memcpy(buf, value, (size_t)n);
  return n;
}
