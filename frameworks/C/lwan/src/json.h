/*
 * Copyright (c) 2017 Intel Corporation
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef ZEPHYR_INCLUDE_DATA_JSON_H_
#define ZEPHYR_INCLUDE_DATA_JSON_H_

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ROUND_UP(x, align)                                                     \
    (((unsigned long)(x) + ((unsigned long)(align)-1)) &                       \
     ~((unsigned long)(align)-1))

/**
 * @brief Structured Data
 * @defgroup structured_data Structured Data
 */

/**
 * @defgroup json JSON
 * @ingroup structured_data
 * @{
 */

enum json_tokens {
    /* Before changing this enum, ensure that its maximum
     * value is still within 7 bits. See comment next to the
     * declaration of `type` in struct json_obj_descr.
     */

    JSON_TOK_NONE = '_',
    JSON_TOK_OBJECT_START = '{',
    JSON_TOK_OBJECT_END = '}',
    JSON_TOK_LIST_START = '[',
    JSON_TOK_LIST_END = ']',
    JSON_TOK_STRING = '"',
    JSON_TOK_COLON = ':',
    JSON_TOK_COMMA = ',',
    JSON_TOK_NUMBER = '0',
    JSON_TOK_TRUE = 't',
    JSON_TOK_FALSE = 'f',
    JSON_TOK_NULL = 'n',
    JSON_TOK_ERROR = '!',
    JSON_TOK_EOF = '\0',
};

struct json_obj_descr {
    const char *field_name;

    uint32_t align;
    uint32_t field_name_len;
    uint32_t type;
    uint32_t offset;

    union {
        struct {
            const struct json_obj_descr *sub_descr;
            size_t sub_descr_len;
        } object;
        struct {
            const struct json_obj_descr *element_descr;
            size_t n_elements;
        } array;
    };
};

/**
 * @brief Function pointer type to append bytes to a buffer while
 * encoding JSON data.
 *
 * @param bytes Contents to write to the output
 * @param len Number of bytes in @param bytes to append to output
 * @param data User-provided pointer
 *
 * @return This callback function should return a negative number on
 * error (which will be propagated to the return value of
 * json_obj_encode()), or 0 on success.
 */
typedef int (*json_append_bytes_t)(const char *bytes, size_t len, void *data);

#define JSON_FIELD_NAME(field_name_)                                           \
    .field_name = #field_name_ "\"" #field_name_ "\":",                        \
    .field_name_len = sizeof(#field_name_) - 1

/**
 * @brief Helper macro to declare a descriptor for supported primitive
 * values.
 *
 * @param struct_ Struct packing the values
 *
 * @param field_name_ Field name in the struct
 *
 * @param type_ Token type for JSON value corresponding to a primitive
 * type. Must be one of: JSON_TOK_STRING for strings, JSON_TOK_NUMBER
 * for numbers, JSON_TOK_TRUE (or JSON_TOK_FALSE) for booleans.
 *
 * Here's an example of use:
 *
 *     struct foo {
 *         int some_int;
 *     };
 *
 *     struct json_obj_descr foo[] = {
 *         JSON_OBJ_DESCR_PRIM(struct foo, some_int, JSON_TOK_NUMBER),
 *     };
 */
#define JSON_OBJ_DESCR_PRIM(struct_, field_name_, type_)                       \
    {                                                                          \
        JSON_FIELD_NAME(field_name_),                                          \
            .align = __alignof__(struct_), .type = type_,                      \
            .offset = offsetof(struct_, field_name_),                          \
    }

/**
 * @brief Helper macro to declare a descriptor for an object value
 *
 * @param struct_ Struct packing the values
 *
 * @param field_name_ Field name in the struct
 *
 * @param sub_descr_ Array of json_obj_descr describing the subobject
 *
 * Here's an example of use:
 *
 *      struct nested {
 *          int foo;
 *          struct {
 *             int baz;
 *          } bar;
 *      };
 *
 *      struct json_obj_descr nested_bar[] = {
 *          { ... declare bar.baz descriptor ... },
 *      };
 *      struct json_obj_descr nested[] = {
 *          { ... declare foo descriptor ... },
 *          JSON_OBJ_DESCR_OBJECT(struct nested, bar, nested_bar),
 *      };
 */
#define JSON_OBJ_DESCR_OBJECT(struct_, field_name_, sub_descr_)                \
    {                                                                          \
        JSON_FIELD_NAME(field_name_),                                          \
            .align = __alignof__(struct_), .type = JSON_TOK_OBJECT_START,      \
            .offset = offsetof(struct_, field_name_),                          \
            .object = {                                                        \
                .sub_descr = sub_descr_,                                       \
                .sub_descr_len = ARRAY_SIZE(sub_descr_),                       \
            },                                                                 \
    }

/**
 * @brief Helper macro to declare a descriptor for an array of primitives
 *
 * @param struct_ Struct packing the values
 *
 * @param field_name_ Field name in the struct
 *
 * @param max_len_ Maximum number of elements in array
 *
 * @param len_field_ Field name in the struct for the number of elements
 * in the array
 *
 * @param elem_type_ Element type, must be a primitive type
 *
 * Here's an example of use:
 *
 *      struct example {
 *          int foo[10];
 *          size_t foo_len;
 *      };
 *
 *      struct json_obj_descr array[] = {
 *           JSON_OBJ_DESCR_ARRAY(struct example, foo, 10, foo_len,
 *                                JSON_TOK_NUMBER)
 *      };
 */
#define JSON_OBJ_DESCR_ARRAY(struct_, field_name_, max_len_, len_field_,       \
                             elem_type_)                                       \
    {                                                                          \
        JSON_FIELD_NAME(field_name_),                                          \
            .align = __alignof__(struct_), .type = JSON_TOK_LIST_START,        \
            .offset = offsetof(struct_, field_name_),                          \
            .array = {                                                         \
                .element_descr =                                               \
                    &(struct json_obj_descr){                                  \
                        .align = __alignof__(struct_),                         \
                        .type = elem_type_,                                    \
                        .offset = offsetof(struct_, len_field_),               \
                    },                                                         \
                .n_elements = (max_len_),                                      \
            },                                                                 \
    }

/**
 * @brief Helper macro to declare a descriptor for an array of objects
 *
 * @param struct_ Struct packing the values
 *
 * @param field_name_ Field name in the struct containing the array
 *
 * @param max_len_ Maximum number of elements in the array
 *
 * @param len_field_ Field name in the struct for the number of elements
 * in the array
 *
 * @param elem_descr_ Element descriptor, pointer to a descriptor array
 *
 * @param elem_descr_len_ Number of elements in elem_descr_
 *
 * Here's an example of use:
 *
 *      struct person_height {
 *          const char *name;
 *          int height;
 *      };
 *
 *      struct people_heights {
 *          struct person_height heights[10];
 *          size_t heights_len;
 *      };
 *
 *      struct json_obj_descr person_height_descr[] = {
 *           JSON_OBJ_DESCR_PRIM(struct person_height, name, JSON_TOK_STRING),
 *           JSON_OBJ_DESCR_PRIM(struct person_height, height, JSON_TOK_NUMBER),
 *      };
 *
 *      struct json_obj_descr array[] = {
 *           JSON_OBJ_DESCR_OBJ_ARRAY(struct people_heights, heights, 10,
 *                                    heights_len, person_height_descr,
 *                                    ARRAY_SIZE(person_height_descr)),
 *      };
 */
#define JSON_OBJ_DESCR_OBJ_ARRAY(struct_, field_name_, max_len_, len_field_,   \
                                 elem_descr_, elem_descr_len_)                 \
    {                                                                          \
        JSON_FIELD_NAME(field_name_),                                          \
            .align = __alignof__(struct_), .type = JSON_TOK_LIST_START,        \
            .offset = offsetof(struct_, field_name_),                          \
            .array = {                                                         \
                .element_descr =                                               \
                    &(struct json_obj_descr){                                  \
                        .align = __alignof__(struct_),                         \
                        .type = JSON_TOK_OBJECT_START,                         \
                        .offset = offsetof(struct_, len_field_),               \
                        .object =                                              \
                            {                                                  \
                                .sub_descr = elem_descr_,                      \
                                .sub_descr_len = elem_descr_len_,              \
                            },                                                 \
                    },                                                         \
                .n_elements = (max_len_),                                      \
            },                                                                 \
    }

/**
 * @brief Helper macro to declare a descriptor for an array of array
 *
 * @param struct_ Struct packing the values
 *
 * @param field_name_ Field name in the struct containing the array
 *
 * @param max_len_ Maximum number of elements in the array
 *
 * @param len_field_ Field name in the struct for the number of elements
 * in the array
 *
 * @param elem_descr_ Element descriptor, pointer to a descriptor array
 *
 * @param elem_descr_len_ Number of elements in elem_descr_
 *
 * Here's an example of use:
 *
 *      struct person_height {
 *          const char *name;
 *          int height;
 *      };
 *
 *      struct person_heights_array {
 *          struct person_height heights;
 *      }
 *
 *      struct people_heights {
 *          struct person_height_array heights[10];
 *          size_t heights_len;
 *      };
 *
 *      struct json_obj_descr person_height_descr[] = {
 *          JSON_OBJ_DESCR_PRIM(struct person_height, name, JSON_TOK_STRING),
 *          JSON_OBJ_DESCR_PRIM(struct person_height, height, JSON_TOK_NUMBER),
 *      };
 *
 *      struct json_obj_descr person_height_array_descr[] = {
 *          JSON_OBJ_DESCR_OBJECT(struct person_heights_array,
 *                                heights, person_heigth_descr),
 *      };
 *
 *      struct json_obj_descr array_array[] = {
 *           JSON_OBJ_DESCR_ARRAY_ARRAY(struct people_heights, heights, 10,
 *                                      heights_len, person_height_array_descr,
 *                                      ARRAY_SIZE(person_height_array_descr)),
 *      };
 */
#define JSON_OBJ_DESCR_ARRAY_ARRAY(struct_, field_name_, max_len_, len_field_, \
                                   elem_descr_, elem_descr_len_)               \
    {                                                                          \
        JSON_FIELD_NAME(field_name_),                                          \
            .align = __alignof__(struct_), .type = JSON_TOK_LIST_START,        \
            .offset = offsetof(struct_, field_name_),                          \
            .array = {                                                         \
                .element_descr =                                               \
                    &(struct json_obj_descr){                                  \
                        .align = __alignof__(struct_),                         \
                        .type = JSON_TOK_LIST_START,                           \
                        .offset = offsetof(struct_, len_field_),               \
                        .object =                                              \
                            {                                                  \
                                .sub_descr = elem_descr_,                      \
                                .sub_descr_len = elem_descr_len_,              \
                            },                                                 \
                    },                                                         \
                .n_elements = (max_len_),                                      \
            },                                                                 \
    }

/**
 * @brief Variant of JSON_OBJ_DESCR_PRIM that can be used when the
 *        structure and JSON field names differ.
 *
 * This is useful when the JSON field is not a valid C identifier.
 *
 * @param struct_ Struct packing the values.
 *
 * @param json_field_name_ String, field name in JSON strings
 *
 * @param struct_field_name_ Field name in the struct
 *
 * @param type_ Token type for JSON value corresponding to a primitive
 * type.
 *
 * @see JSON_OBJ_DESCR_PRIM
 */
#define JSON_OBJ_DESCR_PRIM_NAMED(struct_, json_field_name_,                   \
                                  struct_field_name_, type_)                   \
    {                                                                          \
        JSON_FIELD_NAME(json_field_name_),                                     \
            .align = __alignof__(struct_), .type = type_,                      \
            .offset = offsetof(struct_, struct_field_name_),                   \
    }

/**
 * @brief Variant of JSON_OBJ_DESCR_OBJECT that can be used when the
 *        structure and JSON field names differ.
 *
 * This is useful when the JSON field is not a valid C identifier.
 *
 * @param struct_ Struct packing the values
 *
 * @param json_field_name_ String, field name in JSON strings
 *
 * @param struct_field_name_ Field name in the struct
 *
 * @param sub_descr_ Array of json_obj_descr describing the subobject
 *
 * @see JSON_OBJ_DESCR_OBJECT
 */
#define JSON_OBJ_DESCR_OBJECT_NAMED(struct_, json_field_name_,                 \
                                    struct_field_name_, sub_descr_)            \
    {                                                                          \
        JSON_FIELD_NAME(json_field_name_),                                     \
            .align = __alignof__(struct_), .type = JSON_TOK_OBJECT_START,      \
            .offset = offsetof(struct_, struct_field_name_),                   \
            .object = {                                                        \
                .sub_descr = sub_descr_,                                       \
                .sub_descr_len = ARRAY_SIZE(sub_descr_),                       \
            },                                                                 \
    }

/**
 * @brief Variant of JSON_OBJ_DESCR_ARRAY that can be used when the
 *        structure and JSON field names differ.
 *
 * This is useful when the JSON field is not a valid C identifier.
 *
 * @param struct_ Struct packing the values
 *
 * @param json_field_name_ String, field name in JSON strings
 *
 * @param struct_field_name_ Field name in the struct
 *
 * @param max_len_ Maximum number of elements in array
 *
 * @param len_field_ Field name in the struct for the number of elements
 * in the array
 *
 * @param elem_type_ Element type, must be a primitive type
 *
 * @see JSON_OBJ_DESCR_ARRAY
 */
#define JSON_OBJ_DESCR_ARRAY_NAMED(struct_, json_field_name_,                  \
                                   struct_field_name_, max_len_, len_field_,   \
                                   elem_type_)                                 \
    {                                                                          \
        JSON_FIELD_NAME(json_field_name_),                                     \
            .align = __alignof__(struct_), .type = JSON_TOK_LIST_START,        \
            .offset = offsetof(struct_, struct_field_name_),                   \
            .array = {                                                         \
                .element_descr =                                               \
                    &(struct json_obj_descr){                                  \
                        .align = __alignof__(struct_),                         \
                        .type = elem_type_,                                    \
                        .offset = offsetof(struct_, len_field_),               \
                    },                                                         \
                .n_elements = (max_len_),                                      \
            },                                                                 \
    }

/**
 * @brief Variant of JSON_OBJ_DESCR_OBJ_ARRAY that can be used when
 *        the structure and JSON field names differ.
 *
 * This is useful when the JSON field is not a valid C identifier.
 *
 * @param struct_ Struct packing the values
 *
 * @param json_field_name_ String, field name of the array in JSON strings
 *
 * @param struct_field_name_ Field name in the struct containing the array
 *
 * @param max_len_ Maximum number of elements in the array
 *
 * @param len_field_ Field name in the struct for the number of elements
 * in the array
 *
 * @param elem_descr_ Element descriptor, pointer to a descriptor array
 *
 * @param elem_descr_len_ Number of elements in elem_descr_
 *
 * Here's an example of use:
 *
 *      struct person_height {
 *          const char *name;
 *          int height;
 *      };
 *
 *      struct people_heights {
 *          struct person_height heights[10];
 *          size_t heights_len;
 *      };
 *
 *      struct json_obj_descr person_height_descr[] = {
 *           JSON_OBJ_DESCR_PRIM(struct person_height, name, JSON_TOK_STRING),
 *           JSON_OBJ_DESCR_PRIM(struct person_height, height, JSON_TOK_NUMBER),
 *      };
 *
 *      struct json_obj_descr array[] = {
 *           JSON_OBJ_DESCR_OBJ_ARRAY_NAMED(struct people_heights,
 *                                          "people-heights", heights,
 *                                          10, heights_len,
 *                                          person_height_descr,
 *                                          ARRAY_SIZE(person_height_descr)),
 *      };
 */
#define JSON_OBJ_DESCR_OBJ_ARRAY_NAMED(                                        \
    struct_, json_field_name_, struct_field_name_, max_len_, len_field_,       \
    elem_descr_, elem_descr_len_)                                              \
    {                                                                          \
        JSON_FIELD_NAME(json_field_name_),                                     \
            .align = __alignof__(struct_), .type = JSON_TOK_LIST_START,        \
            .offset = offsetof(struct_, struct_field_name_),                   \
            .element_descr =                                                   \
                &(struct json_obj_descr){                                      \
                    .align = __alignof__(struct_),                             \
                    .type = JSON_TOK_OBJECT_START,                             \
                    .offset = offsetof(struct_, len_field_),                   \
                    .object =                                                  \
                        {                                                      \
                            .sub_descr = elem_descr_,                          \
                            .sub_descr_len = elem_descr_len_,                  \
                        },                                                     \
                },                                                             \
            .n_elements = (max_len_),                                          \
    }

/**
 * @brief Parses the JSON-encoded object pointer to by @a json, with
 * size @a len, according to the descriptor pointed to by @a descr.
 * Values are stored in a struct pointed to by @a val.  Set up the
 * descriptor like this:
 *
 *    struct s { int foo; char *bar; }
 *    struct json_obj_descr descr[] = {
 *       JSON_OBJ_DESCR_PRIM(struct s, foo, JSON_TOK_NUMBER),
 *       JSON_OBJ_DESCR_PRIM(struct s, bar, JSON_TOK_STRING),
 *    };
 *
 * Since this parser is designed for machine-to-machine communications, some
 * liberties were taken to simplify the design:
 * (1) strings are not unescaped (but only valid escape sequences are
 * accepted);
 * (2) no UTF-8 validation is performed; and
 * (3) only integer numbers are supported (no strtod() in the minimal libc).
 *
 * @param json Pointer to JSON-encoded value to be parsed
 *
 * @param len Length of JSON-encoded value
 *
 * @param descr Pointer to the descriptor array
 *
 * @param descr_len Number of elements in the descriptor array. Must be less
 * than 31 due to implementation detail reasons (if more fields are
 * necessary, use two descriptors)
 *
 * @param val Pointer to the struct to hold the decoded values
 *
 * @return < 0 if error, bitmap of decoded fields on success (bit 0
 * is set if first field in the descriptor has been properly decoded, etc).
 */
int json_obj_parse(char *json,
                   size_t len,
                   const struct json_obj_descr *descr,
                   size_t descr_len,
                   void *val);

/**
 * @brief Escapes the string so it can be used to encode JSON objects
 *
 * @param str The string to escape; the escape string is stored the
 * buffer pointed to by this parameter
 *
 * @param len Points to a size_t containing the size before and after
 * the escaping process
 *
 * @param buf_size The size of buffer str points to
 *
 * @return 0 if string has been escaped properly, or -ENOMEM if there
 * was not enough space to escape the buffer
 */
ssize_t json_escape(char *str, size_t *len, size_t buf_size);

/**
 * @brief Calculates the JSON-escaped string length
 *
 * @param str The string to analyze
 *
 * @param len String size
 *
 * @return The length str would have if it were escaped
 */
size_t json_calc_escaped_len(const char *str, size_t len);

/**
 * @brief Calculates the string length to fully encode an object
 *
 * @param descr Pointer to the descriptor array
 *
 * @param descr_len Number of elements in the descriptor array
 *
 * @param val Struct holding the values
 *
 * @return Number of bytes necessary to encode the values if >0,
 * an error code is returned.
 */
ssize_t json_calc_encoded_len(const struct json_obj_descr *descr,
                              size_t descr_len,
                              const void *val);

/**
 * @brief Encodes an object in a contiguous memory location
 *
 * @param descr Pointer to the descriptor array
 *
 * @param descr_len Number of elements in the descriptor array
 *
 * @param val Struct holding the values
 *
 * @param buffer Buffer to store the JSON data
 *
 * @param buf_size Size of buffer, in bytes, with space for the terminating
 * NUL character
 *
 * @return 0 if object has been successfully encoded. A negative value
 * indicates an error (as defined on errno.h).
 */
int json_obj_encode_buf(const struct json_obj_descr *descr,
                        size_t descr_len,
                        const void *val,
                        char *buffer,
                        size_t buf_size);

/**
 * @brief Encodes an object using an arbitrary writer function
 *
 * @param descr Pointer to the descriptor array
 *
 * @param descr_len Number of elements in the descriptor array
 *
 * @param val Struct holding the values
 *
 * @param append_bytes Function to append bytes to the output
 *
 * @param data Data pointer to be passed to the append_bytes callback
 * function.
 *
 * @return 0 if object has been successfully encoded. A negative value
 * indicates an error.
 */
int json_obj_encode_full(const struct json_obj_descr *descr,
                         size_t descr_len,
                         const void *val,
                         json_append_bytes_t append_bytes,
                         void *data,
                         bool escape_key);
static inline int json_obj_encode(const struct json_obj_descr *descr,
                                  size_t descr_len,
                                  const void *val,
                                  json_append_bytes_t append_bytes,
                                  void *data)
{

    return json_obj_encode_full(descr, descr_len, val, append_bytes, data,
                                true);
}
/**
 * @brief Encodes an array using an arbitrary writer function
 *
 * @param descr Pointer to the descriptor array
 *
 * @param descr_len Number of elements in the descriptor array
 *
 * @param val Struct holding the values
 *
 * @param append_bytes Function to append bytes to the output
 *
 * @param data Data pointer to be passed to the append_bytes callback
 * function.
 *
 * @return 0 if object has been successfully encoded. A negative value
 * indicates an error.
 */
int json_arr_encode_full(const struct json_obj_descr *descr,
                         const void *val,
                         json_append_bytes_t append_bytes,
                         void *data,
                         bool escape_key);
static inline int json_arr_encode(const struct json_obj_descr *descr,
                                  const void *val,
                                  json_append_bytes_t append_bytes,
                                  void *data)
{
    return json_arr_encode_full(descr, val, append_bytes, data, true);
}

#ifdef __cplusplus
}
#endif

/**
 * @}
 */
#endif /* ZEPHYR_INCLUDE_DATA_JSON_H_ */
