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

// ============================================================================
// Threading
// ============================================================================

/**
 * Spawns a new OS thread that begins execution at start_routine(arg).
 *
 * @param start_routine Function pointer to the thread entry point
 * @param arg Argument passed to start_routine
 * @return 0 on success, non-zero on failure
 */
int ion_spawn(void *(*start_routine)(void *), void *arg);

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
 * Returns a tuple of (Sender, Receiver) handles.
 *
 * @param elem_size Size of each element in bytes
 * @param capacity Maximum number of elements that can be buffered
 * @param sender_out Output parameter for the sender handle
 * @param receiver_out Output parameter for the receiver handle
 * @return 0 on success, non-zero on failure
 */
int ion_channel_new(size_t elem_size, int capacity, ion_sender_t *sender_out,
                    ion_receiver_t *receiver_out);

/**
 * Sends a value into the channel using a sender handle. Blocks if the buffer is
 * full.
 *
 * @param sender Sender handle
 * @param value Pointer to the value to send (must be elem_size bytes)
 * @return 0 on success, non-zero on failure (e.g., channel closed)
 */
int ion_channel_send(const ion_sender_t *sender, const void *value);

/**
 * Receives a value from the channel using a receiver handle. Blocks if the
 * buffer is empty.
 *
 * @param receiver Receiver handle
 * @param out_value Pointer to buffer to write the received value (must be
 * elem_size bytes)
 * @return 0 on success, non-zero on failure (e.g., channel closed)
 */
int ion_channel_recv(ion_receiver_t *receiver, void *out_value);

// Legacy channel API (deprecated, kept for backward compatibility)
ion_channel_t *ion_channel_new_legacy(size_t elem_size, int capacity);
int ion_channel_send_legacy(ion_channel_t *ch, const void *value);
int ion_channel_recv_legacy(ion_channel_t *ch, void *out_value);

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
  char *data;
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
 *
 * @param lit C string literal (null-terminated)
 * @param len Length of the string (excluding null terminator)
 * @return Pointer to allocated ion_string_t, or NULL on failure
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
 * Appends a string or string literal to the end of a string.
 *
 * @param s String to append to
 * @param other String literal or string to append
 * @param other_len Length of other (0 if other is a string pointer)
 * @return 0 on success, non-zero on failure
 */
int ion_string_push_str(ion_string_t *s, const char *other, size_t other_len);

/**
 * Frees a heap-allocated string.
 * Frees the data buffer and the string structure itself.
 *
 * @param s Pointer to string to free
 */
void ion_string_free(ion_string_t *s);

#ifdef __cplusplus
}
#endif

#endif // ION_RUNTIME_H
