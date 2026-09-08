#include "ion_runtime.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <winsock2.h>
#pragma comment(lib, "ws2_32.lib")
#endif

// ============================================================================
// Safety and Error Handling
// ============================================================================

void ion_panic(const char *message) {
  fprintf(stderr, "Ion panic: %s\n", message);
  abort();
}

// ============================================================================
// Heap Allocation
// ============================================================================

void *ion_box_alloc(size_t size) { return malloc(size); }

void ion_box_free(void *ptr) { free(ptr); }

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

int ion_vec_push(ion_vec_t *vec, const void *value, size_t elem_size) {
  if (!vec || !value)
    return -1;
  if (elem_size != vec->elem_size)
    return -1;

  // Grow if needed
  if (vec->len >= vec->capacity) {
    size_t new_capacity = vec->capacity == 0 ? 4 : vec->capacity * 2;
    void *new_data = realloc(vec->data, elem_size * new_capacity);
    if (!new_data)
      return -1;
    vec->data = new_data;
    vec->capacity = new_capacity;
  }

  // Copy value to end
  memcpy((char *)vec->data + (vec->len * elem_size), value, elem_size);
  vec->len++;

  return 0;
}

// Option enum structure (tagged union)
// Option<T> = { tag: 0 (None) } | { tag: 1 (Some), data: T }
typedef struct {
  int tag; // 0 = None, 1 = Some
  union {
    char some_data[1]; // Variable-sized, actual size is elem_size
  } data;
} ion_option_t;

void *ion_vec_pop(ion_vec_t *vec, size_t elem_size) {
  if (!vec || vec->len == 0) {
    // Return Option::None (tag = 1, since None is second variant)
    ion_option_t *opt = (ion_option_t *)malloc(sizeof(ion_option_t));
    if (!opt)
      return NULL;
    opt->tag = 1; // None
    return opt;
  }

  // Return Option::Some(value) (tag = 0, since Some is first variant)
  size_t opt_size = sizeof(ion_option_t) - 1 + elem_size;
  ion_option_t *opt = (ion_option_t *)malloc(opt_size);
  if (!opt)
    return NULL;

  opt->tag = 0; // Some
  // Copy the last element
  vec->len--;
  memcpy(opt->data.some_data, (char *)vec->data + (vec->len * elem_size),
         elem_size);

  return opt;
}

void ion_option_from_raw(void *dest, void *raw, size_t elem_size,
                         size_t payload_offset) {
  if (!dest) {
    free(raw);
    return;
  }
  if (!raw) {
    *(int *)dest = 1;
    return;
  }
  ion_option_t *src = (ion_option_t *)raw;
  *(int *)dest = src->tag;
  if (src->tag == 0) {
    memcpy((char *)dest + payload_offset, src->data.some_data, elem_size);
  }
  free(raw);
}

void *ion_vec_get(const ion_vec_t *vec, int index, size_t elem_size) {
  if (!vec || index < 0 || (size_t)index >= vec->len) {
    // Return Option::None (tag = 1, since None is second variant)
    ion_option_t *opt = (ion_option_t *)malloc(sizeof(ion_option_t));
    if (!opt)
      return NULL;
    opt->tag = 1; // None
    return opt;
  }

  // Return Option::Some(value) (tag = 0, since Some is first variant)
  size_t opt_size = sizeof(ion_option_t) - 1 + elem_size;
  ion_option_t *opt = (ion_option_t *)malloc(opt_size);
  if (!opt)
    return NULL;

  opt->tag = 0; // Some
  memcpy(opt->data.some_data, (char *)vec->data + (index * elem_size),
         elem_size);

  return opt;
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

  // Grow if needed
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

  // Append
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

int ion_spawn(void *(*start_routine)(void *), void *arg) {
  pthread_t thread;
  int rc = pthread_create(&thread, NULL, start_routine, arg);
  if (rc != 0)
    return rc;
  pthread_detach(thread);
  return 0;
}

// ============================================================================
// Channel Implementation
// ============================================================================

struct ion_channel_t {
  void *buffer;
  size_t elem_size;
  int capacity;
  int head;
  int tail;
  int count;
  pthread_mutex_t mutex;
  pthread_cond_t not_full;
  pthread_cond_t not_empty;
  int sender_count;
  int receiver_count;
  int recv_closed;
  int send_closed;
  void (*drop_fn)(void *);
};

static void ion_channel_drop_buffered(struct ion_channel_t *ch) {
  if (!ch->drop_fn || !ch->buffer)
    return;
  while (ch->count > 0) {
    void *slot = (char *)ch->buffer + (ch->head * ch->elem_size);
    ch->drop_fn(slot);
    ch->head = (ch->head + 1) % ch->capacity;
    ch->count--;
  }
}

static void ion_channel_destroy(struct ion_channel_t *ch) {
  if (!ch)
    return;
  ion_channel_drop_buffered(ch);
  if (ch->buffer)
    free(ch->buffer);
  pthread_cond_destroy(&ch->not_full);
  pthread_cond_destroy(&ch->not_empty);
  pthread_mutex_destroy(&ch->mutex);
  free(ch);
}

int ion_channel_new(size_t elem_size, int capacity, void (*drop_fn)(void *),
                    ion_sender_t *sender_out, ion_receiver_t *receiver_out) {
  if (!sender_out || !receiver_out)
    return -1;
  if (capacity < 1)
    ion_panic("channel capacity must be >= 1");

  struct ion_channel_t *ch =
      (struct ion_channel_t *)malloc(sizeof(struct ion_channel_t));
  if (!ch)
    return -1;

  ch->elem_size = elem_size;
  ch->capacity = capacity;
  ch->head = 0;
  ch->tail = 0;
  ch->count = 0;
  ch->sender_count = 1;
  ch->receiver_count = 1;
  ch->recv_closed = 0;
  ch->send_closed = 0;
  ch->drop_fn = drop_fn;

  ch->buffer = malloc(elem_size * (size_t)ch->capacity);
  if (!ch->buffer) {
    free(ch);
    return -1;
  }

  if (pthread_mutex_init(&ch->mutex, NULL) != 0) {
    free(ch->buffer);
    free(ch);
    return -1;
  }

  if (pthread_cond_init(&ch->not_full, NULL) != 0) {
    pthread_mutex_destroy(&ch->mutex);
    free(ch->buffer);
    free(ch);
    return -1;
  }

  if (pthread_cond_init(&ch->not_empty, NULL) != 0) {
    pthread_cond_destroy(&ch->not_full);
    pthread_mutex_destroy(&ch->mutex);
    free(ch->buffer);
    free(ch);
    return -1;
  }

  sender_out->channel = (ion_channel_t *)ch;
  sender_out->elem_size = elem_size;
  receiver_out->channel = (ion_channel_t *)ch;
  receiver_out->elem_size = elem_size;

  return 0;
}

int ion_channel_send(const ion_sender_t *sender, const void *value) {
  if (!sender || !sender->channel || !value)
    return -1;

  struct ion_channel_t *ch = (struct ion_channel_t *)sender->channel;

  pthread_mutex_lock(&ch->mutex);

  while (ch->count >= ch->capacity && !ch->send_closed) {
    pthread_cond_wait(&ch->not_full, &ch->mutex);
  }

  if (ch->send_closed) {
    pthread_mutex_unlock(&ch->mutex);
    return -1;
  }

  memcpy((char *)ch->buffer + (ch->tail * ch->elem_size), value, ch->elem_size);
  ch->tail = (ch->tail + 1) % ch->capacity;
  ch->count++;

  pthread_cond_signal(&ch->not_empty);
  pthread_mutex_unlock(&ch->mutex);

  return 0;
}

int ion_channel_recv(ion_receiver_t *receiver, void *out_value) {
  if (!receiver || !receiver->channel || !out_value)
    return -1;

  struct ion_channel_t *ch = (struct ion_channel_t *)receiver->channel;

  pthread_mutex_lock(&ch->mutex);

  while (ch->count == 0 && !ch->recv_closed) {
    pthread_cond_wait(&ch->not_empty, &ch->mutex);
  }

  if (ch->count == 0 && ch->recv_closed) {
    pthread_mutex_unlock(&ch->mutex);
    return -1;
  }

  memcpy(out_value, (char *)ch->buffer + (ch->head * ch->elem_size),
         ch->elem_size);
  ch->head = (ch->head + 1) % ch->capacity;
  ch->count--;

  pthread_cond_signal(&ch->not_full);
  pthread_mutex_unlock(&ch->mutex);

  return 0;
}

int ion_channel_clone_sender(const ion_sender_t *src, ion_sender_t *dst) {
  if (!src || !src->channel || !dst)
    return -1;

  struct ion_channel_t *ch = (struct ion_channel_t *)src->channel;
  pthread_mutex_lock(&ch->mutex);
  ch->sender_count++;
  pthread_mutex_unlock(&ch->mutex);

  dst->channel = src->channel;
  dst->elem_size = src->elem_size;
  return 0;
}

void ion_channel_sender_drop(ion_sender_t *sender) {
  if (!sender || !sender->channel)
    return;

  struct ion_channel_t *ch = (struct ion_channel_t *)sender->channel;
  int destroy = 0;

  pthread_mutex_lock(&ch->mutex);
  ch->sender_count--;
  if (ch->sender_count <= 0) {
    ch->recv_closed = 1;
    pthread_cond_broadcast(&ch->not_empty);
  }
  if (ch->sender_count <= 0 && ch->receiver_count <= 0)
    destroy = 1;
  sender->channel = NULL;
  pthread_mutex_unlock(&ch->mutex);

  if (destroy)
    ion_channel_destroy(ch);
}

void ion_channel_receiver_drop(ion_receiver_t *receiver) {
  if (!receiver || !receiver->channel)
    return;

  struct ion_channel_t *ch = (struct ion_channel_t *)receiver->channel;
  int destroy = 0;

  pthread_mutex_lock(&ch->mutex);
  ch->receiver_count--;
  if (ch->receiver_count <= 0) {
    ch->send_closed = 1;
    pthread_cond_broadcast(&ch->not_full);
  }
  if (ch->sender_count <= 0 && ch->receiver_count <= 0)
    destroy = 1;
  receiver->channel = NULL;
  pthread_mutex_unlock(&ch->mutex);

  if (destroy)
    ion_channel_destroy(ch);
}
