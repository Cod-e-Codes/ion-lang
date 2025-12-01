#include "ion_runtime.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

  s->data = (char *)malloc(1);
  if (!s->data) {
    free(s);
    return NULL;
  }
  s->data[0] = '\0';
  s->len = 0;
  s->capacity = 1;

  return s;
}

ion_string_t *ion_string_from_literal(const char *lit, size_t len) {
  ion_string_t *s = (ion_string_t *)malloc(sizeof(ion_string_t));
  if (!s)
    return NULL;

  s->data = (char *)malloc(len + 1);
  if (!s->data) {
    free(s);
    return NULL;
  }
  memcpy(s->data, lit, len);
  s->data[len] = '\0';
  s->len = len;
  s->capacity = len + 1;

  return s;
}

ion_string_t *ion_string_clone(const ion_string_t *s) {
  if (!s)
    return NULL;
  return ion_string_from_literal(s->data, s->len);
}

int ion_string_push_str(ion_string_t *s, const char *other, size_t other_len) {
  if (!s || !other)
    return -1;

  // If other_len is 0, treat other as a string pointer and get its length
  size_t append_len = other_len;
  if (append_len == 0) {
    append_len = strlen(other);
  }

  // Grow if needed
  if (s->len + append_len + 1 > s->capacity) {
    size_t new_capacity = s->capacity;
    while (new_capacity < s->len + append_len + 1) {
      new_capacity *= 2;
    }
    char *new_data = (char *)realloc(s->data, new_capacity);
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

void ion_string_free(ion_string_t *s) {
  if (!s)
    return;
  if (s->data)
    free(s->data);
  free(s);
}

// ============================================================================
// Channel Implementation
// ============================================================================

struct ion_channel_t {
  void *buffer;     // Circular buffer
  size_t elem_size; // Size of each element
  int capacity;     // Maximum number of elements
  int head;         // Read position
  int tail;         // Write position
  int count;        // Current number of elements
  pthread_mutex_t mutex;
  pthread_cond_t not_full;  // Signaled when space available
  pthread_cond_t not_empty; // Signaled when data available
  int closed;               // 1 if channel is closed
};

int ion_channel_new(size_t elem_size, int capacity, ion_sender_t *sender_out,
                    ion_receiver_t *receiver_out) {
  if (!sender_out || !receiver_out || capacity < 0)
    return -1;

  struct ion_channel_t *ch =
      (struct ion_channel_t *)malloc(sizeof(struct ion_channel_t));
  if (!ch)
    return -1;

  ch->elem_size = elem_size;
  ch->capacity = capacity > 0 ? capacity : 1;
  ch->head = 0;
  ch->tail = 0;
  ch->count = 0;
  ch->closed = 0;

  ch->buffer = malloc(elem_size * ch->capacity);
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

  // Initialize sender and receiver handles
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

  // Wait until there's space or channel is closed
  while (ch->count >= ch->capacity && !ch->closed) {
    pthread_cond_wait(&ch->not_full, &ch->mutex);
  }

  if (ch->closed) {
    pthread_mutex_unlock(&ch->mutex);
    return -1;
  }

  // Copy value into buffer
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

  // Wait until there's data or channel is closed
  while (ch->count == 0 && !ch->closed) {
    pthread_cond_wait(&ch->not_empty, &ch->mutex);
  }

  if (ch->closed && ch->count == 0) {
    pthread_mutex_unlock(&ch->mutex);
    return -1;
  }

  // Copy value from buffer
  memcpy(out_value, (char *)ch->buffer + (ch->head * ch->elem_size),
         ch->elem_size);
  ch->head = (ch->head + 1) % ch->capacity;
  ch->count--;

  pthread_cond_signal(&ch->not_full);
  pthread_mutex_unlock(&ch->mutex);

  return 0;
}
