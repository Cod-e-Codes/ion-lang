#ifndef ION_RUNTIME_H
#define ION_RUNTIME_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// ============================================================================
// Safety and Error Handling
// ============================================================================

/**
 * Panics the program with an error message.
 * Prints message to stderr and aborts the process.
 * Used for safety violations like array bounds checking failures.
 *
 * @param message Error message to display
 */
void ion_panic(const char *message);

/**
 * Initializes platform networking (WSAStartup on Windows, no-op elsewhere).
 * Call once before socket FFI in programs that use BSD sockets.
 */
void ion_net_init(void);

// ============================================================================
// Threading
// ============================================================================

/**
 * Spawns a new OS thread that begins execution at start_routine(arg).
 * The thread is detached (same as dropping an unused JoinHandle).
 *
 * @param start_routine Function pointer to the thread entry point
 * @param arg Argument passed to start_routine
 * @return 0 on success, non-zero on failure
 */
int ion_spawn(void *(*start_routine)(void *), void *arg);

/**
 * Joinable thread handle. Not Copy. Send. Drop detaches if still live.
 * `thread` is an opaque pthread_t-sized slot filled by the runtime.
 */
typedef struct {
  unsigned char thread[16];
  int live;
} ion_thread_t;

/**
 * Spawns a joinable OS thread. On success, *out is live and must be joined or
 * detached exactly once.
 *
 * @return 0 on success, non-zero on failure (*out unchanged)
 */
int ion_spawn_joinable(void *(*start_routine)(void *), void *arg,
                       ion_thread_t *out);

/**
 * Waits for the thread to finish and marks the handle not live.
 * @return 0 on success, non-zero if the handle is not live or join fails
 */
int ion_join(ion_thread_t *thread);

/**
 * Detaches a live handle so the thread is not joined. No-op if not live.
 */
void ion_thread_detach(ion_thread_t *thread);

// ============================================================================
// Channels
// ============================================================================

/**
 * Opaque channel type (internal)
 */
typedef struct ion_channel_t ion_channel_t;

/**
 * Sender handle - move-only handle for sending messages
 */
typedef struct {
  ion_channel_t *channel;
  size_t elem_size;
} ion_sender_t;

/**
 * Receiver handle - move-only handle for receiving messages
 */
typedef struct {
  ion_channel_t *channel;
  size_t elem_size;
} ion_receiver_t;

/**
 * Creates a bounded MPSC (multi-producer, single-consumer) channel.
 * Returns a tuple of (Sender, Receiver) handles. Capacity must be >= 1.
 * drop_fn is called on each remaining buffered element when the channel is
 * destroyed (NULL if T needs no destructor). The argument is a pointer to the slot.
 *
 * @param elem_size Size of each element in bytes
 * @param capacity Maximum number of elements that can be buffered (>= 1)
 * @param drop_fn Optional destructor for queued elements
 * @param sender_out Output parameter for the sender handle
 * @param receiver_out Output parameter for the receiver handle
 * @return 0 on success, non-zero on failure
 */
int ion_channel_new(size_t elem_size, int capacity, void (*drop_fn)(void *),
                    ion_sender_t *sender_out, ion_receiver_t *receiver_out);

/**
 * Sends a value into the channel using a sender handle. Blocks if the buffer is
 * full and a receiver still exists.
 *
 * @param sender Sender handle
 * @param value Pointer to the value to send (must be elem_size bytes)
 * @return 0 on success, non-zero if no receiver remains (value is not copied)
 */
int ion_channel_send(const ion_sender_t *sender, const void *value);

/**
 * Receives a value from the channel using a receiver handle. Blocks if the
 * buffer is empty and a sender still exists.
 *
 * @param receiver Receiver handle
 * @param out_value Pointer to buffer to write the received value (must be
 * elem_size bytes)
 * @return 0 on success, non-zero if disconnected and empty (out_value unchanged)
 */
int ion_channel_recv(ion_receiver_t *receiver, void *out_value);

/**
 * Nonblocking send. Does not wait if the buffer is full.
 *
 * @return 0 sent, -1 closed (value not copied), -2 full (value not copied)
 */
int ion_channel_try_send(const ion_sender_t *sender, const void *value);

/**
 * Nonblocking receive. Does not wait if the buffer is empty.
 *
 * @return 0 got a message, -1 closed and empty, -2 empty (still open)
 */
int ion_channel_try_recv(ion_receiver_t *receiver, void *out_value);

/**
 * One select arm: wait to receive into out (elem_size bytes).
 */
typedef struct {
  ion_receiver_t *rx;
  void *out;
} ion_select_arm_t;

/**
 * Wait until one arm can take a message (or is closed-empty), a default poll,
 * or a timeout. The chosen arm copies in the same try_recv (no peek-then-recv).
 * Waiters are registered before the empty recheck so a concurrent send cannot
 * park forever on a message already in the buffer.
 *
 * timeout_ms: 0 = poll (default arm), >0 = wait up to that many milliseconds,
 *             -1 = wait forever (no default or timeout arm). Values < -1 panic.
 * @return arm index if a recv arm ran (message copied or closed-empty with
 *         out unchanged and a closed indication via ion_channel_try_recv
 *         status stored in *status_out when non-NULL), n for default/timeout
 */
int ion_channel_select(ion_select_arm_t *arms, int n, int timeout_ms,
                       int *status_out);

/**
 * Copies a sender handle and increments the sender count.
 *
 * @param src Existing sender
 * @param dst Output handle
 * @return 0 on success, non-zero on failure
 */
int ion_channel_clone_sender(const ion_sender_t *src, ion_sender_t *dst);

/**
 * Drops a sender handle. Last sender disconnects receive.
 */
void ion_channel_sender_drop(ion_sender_t *sender);

/**
 * Drops a receiver handle. Last receiver disconnects send.
 */
void ion_channel_receiver_drop(ion_receiver_t *receiver);

// ============================================================================
// Heap Allocation (for Box<T> and collections)
// ============================================================================

/**
 * Allocates raw memory on the heap for Box<T> and other collection types.
 *
 * @param size Number of bytes to allocate
 * @return Pointer to allocated memory, or NULL on failure
 */
void *ion_box_alloc(size_t size);

/**
 * Frees memory previously allocated by ion_box_alloc.
 *
 * @param ptr Pointer to memory to free (must be from ion_box_alloc)
 */
void ion_box_free(void *ptr);

// ============================================================================
// Vec Type (Generic Vector)
// ============================================================================

/**
 * Generic Vec structure - heap-allocated growable array
 * The actual Vec type is monomorphized per element type (e.g., Vec_int)
 */
typedef struct {
  void *data;       // Pointer to element array
  size_t len;       // Number of elements
  size_t capacity;  // Allocated capacity
  size_t elem_size; // Size of each element in bytes
} ion_vec_t;

/**
 * Creates a new empty vector.
 *
 * @param elem_size Size of each element in bytes
 * @return Pointer to allocated vector, or NULL on failure
 */
ion_vec_t *ion_vec_new(size_t elem_size);

/**
 * Creates a new vector with specified initial capacity.
 *
 * @param elem_size Size of each element in bytes
 * @param capacity Initial capacity
 * @return Pointer to allocated vector, or NULL on failure
 */
ion_vec_t *ion_vec_with_capacity(size_t elem_size, int capacity);

/**
 * Pushes a value onto the end of the vector.
 * The value is copied into the vector.
 *
 * @param vec Vector to push to
 * @param value Pointer to value to copy (must be elem_size bytes)
 * @return 0 on success, non-zero on failure
 */
int ion_vec_push(ion_vec_t *vec, const void *value, size_t elem_size);

/**
 * Pops a value from the end of the vector.
 * Returns an Option<T> enum: Some(T) if successful, None if empty.
 * The caller must handle the Option enum.
 *
 * @param vec Vector to pop from
 * @param elem_size Size of each element in bytes
 * @return Pointer to Option enum (caller must free), or NULL on error
 */
void *ion_vec_pop(ion_vec_t *vec, size_t elem_size);

/**
 * Gets a value from the vector at the given index.
 * Returns an Option<T> enum: Some(T) if index is valid, None if out of bounds.
 *
 * @param vec Vector to get from
 * @param index Index to get
 * @param elem_size Size of each element in bytes
 * @return Pointer to Option enum (caller must free), or NULL on error
 */
void *ion_vec_get(const ion_vec_t *vec, int index, size_t elem_size);

/**
 * Unpack a heap Option from ion_vec_get/ion_vec_pop into a stack-local monomorphized
 * Option (tag + payload). Frees raw.
 */
void ion_option_from_raw(void *dest, void *raw, size_t elem_size,
                         size_t payload_offset);

/**
 * Sets a value in the vector at the given index.
 *
 * @param vec Vector to set in
 * @param index Index to set
 * @param value Pointer to value to copy (must be elem_size bytes)
 * @param elem_size Size of each element in bytes
 * @return 0 on success, -1 if index out of bounds
 */
int ion_vec_set(ion_vec_t *vec, int index, const void *value, size_t elem_size);

/**
 * Frees a vector and its data.
 *
 * @param vec Vector to free
 */
void ion_vec_free(ion_vec_t *vec);

// ============================================================================
// String Type
// ============================================================================

/**
 * String type - heap-allocated UTF-8 string
 */
typedef struct {
  uint8_t *data;
  size_t len;
  size_t capacity;
} ion_string_t;

/**
 * Creates a new empty string.
 *
 * @return Pointer to allocated string, or NULL on failure
 */
ion_string_t *ion_string_new(void);

/**
 * Creates a heap-allocated string from a C string literal.
 * Allocates a new ion_string_t and copies the literal data.
 * lit[0..len) must be well-formed UTF-8 (RFC 3629).
 *
 * @param lit C string literal (null-terminated)
 * @param len Length of the string (excluding null terminator)
 * @return Pointer to allocated ion_string_t, or NULL on failure or invalid UTF-8
 */
ion_string_t *ion_string_from_literal(const char *lit, size_t len);

/**
 * Clones a string, creating a new copy.
 *
 * @param s String to clone
 * @return Pointer to new string copy, or NULL on failure
 */
ion_string_t *ion_string_clone(const ion_string_t *s);

/**
 * Returns 1 if data[0..len) is well-formed UTF-8 (RFC 3629): no overlong
 * encodings, no surrogates, and no code points above U+10FFFF. Empty input
 * is valid. A NULL pointer with len 0 is valid; NULL with len > 0 is not.
 */
int ion_utf8_valid(const uint8_t *data, size_t len);

/**
 * Appends a string or string literal to the end of a string.
 * other[0..other_len) must be well-formed UTF-8.
 *
 * @param s String to append to
 * @param other String literal or string to append
 * @param other_len Length of other (0 if other is a string pointer)
 * @return 0 on success, non-zero on failure
 */
int ion_string_push_str(ion_string_t *s, const char *other, size_t other_len);

/**
 * Appends a single ASCII byte (0x00..=0x7F) to the end of a string.
 * Bytes >= 0x80 are rejected because they would break the UTF-8 invariant.
 *
 * @param s String to append to
 * @param b Byte to append
 * @return 0 on success, non-zero on failure
 */
int ion_string_push_byte(ion_string_t *s, unsigned char b);

/**
 * Compares two strings for UTF-8 byte equality.
 *
 * @param a First string (may be NULL)
 * @param b Second string (may be NULL)
 * @return 1 if both strings have the same length and bytes, 0 otherwise
 */
int ion_string_equals(const ion_string_t *a, const ion_string_t *b);

/**
 * Frees a heap-allocated string.
 * Frees the data buffer and the string structure itself.
 *
 * @param s Pointer to string to free
 */
void ion_string_free(ion_string_t *s);

// ============================================================================
// File
// ============================================================================

/**
 * Owned file handle. Not Send. Drop closes. fp is NULL after close.
 */
typedef struct {
  void *fp;
} ion_file_t;

/**
 * Opens path with fopen mode (e.g. "rb", "w+b"). Returns a handle with fp
 * NULL on failure.
 */
ion_file_t ion_file_open(const char *path, const char *mode);

/**
 * Reads up to n bytes into buf. *out_n is bytes read. Returns 0 on success
 * (including EOF with *out_n == 0), non-zero on error.
 */
int ion_file_read(ion_file_t *file, void *buf, size_t n, size_t *out_n);

/**
 * Writes n bytes from buf. *out_n is bytes written. Returns 0 on success,
 * non-zero on error.
 */
int ion_file_write(ion_file_t *file, const void *buf, size_t n, size_t *out_n);

/**
 * Closes the file if still open and clears fp. Safe to call twice.
 */
void ion_file_close(ion_file_t *file);

#ifdef __cplusplus
}
#endif

#endif // ION_RUNTIME_H
