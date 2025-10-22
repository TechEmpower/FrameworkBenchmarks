/*
 * Copyright (c) 2017 Intel Corporation
 * Copyright (c) 2022 L. A. F. Pereira <l@tia.mat.br>
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "json.h"
#include "lwan.h"
#include "int-to-str.h"

struct token {
    enum json_tokens type;
    char *start;
    char *end;
};

struct lexer {
    void *(*state)(struct lexer *lexer);
    char *start;
    char *pos;
    char *end;
    struct token token;
};

struct json_obj {
    struct lexer lexer;
};

struct json_obj_key_value {
    const char *key;
    size_t key_len;
    struct token value;
};

static bool lexer_consume(struct lexer *lexer,
                          struct token *token,
                          enum json_tokens empty_token)
{
    if (lexer->token.type == empty_token) {
        return false;
    }

    *token = lexer->token;
    lexer->token.type = empty_token;

    return true;
}

static bool lexer_next(struct lexer *lexer, struct token *token)
{
    while (lexer->state) {
        if (lexer_consume(lexer, token, JSON_TOK_NONE)) {
            return true;
        }

        lexer->state = lexer->state(lexer);
    }

    return lexer_consume(lexer, token, JSON_TOK_EOF);
}

static void *lexer_json(struct lexer *lexer);

static void emit(struct lexer *lexer, enum json_tokens token)
{
    lexer->token.type = token;
    lexer->token.start = lexer->start;
    lexer->token.end = lexer->pos;
    lexer->start = lexer->pos;
}

static void* emit_cont(struct lexer *lexer, enum json_tokens token)
{
    emit(lexer, token);
    return lexer_json;
}

static void* emit_end(struct lexer *lexer, enum json_tokens token)
{
    emit(lexer, token);
    return NULL;
}

static int next(struct lexer *lexer)
{
    if (lexer->pos >= lexer->end) {
        lexer->pos = lexer->end + 1;

        return '\0';
    }

    return *lexer->pos++;
}

static void ignore(struct lexer *lexer) { lexer->start = lexer->pos; }

static void backup(struct lexer *lexer) { lexer->pos--; }

static int peek(struct lexer *lexer)
{
    int chr = next(lexer);

    backup(lexer);

    return chr;
}

static void *lexer_string(struct lexer *lexer)
{
    ignore(lexer);

    while (true) {
        int chr = next(lexer);

        if (UNLIKELY(chr == '\0')) {
            return emit_end(lexer, JSON_TOK_ERROR);
        }

        if (chr == '\\') {
            switch (next(lexer)) {
            case '"':
            case '\\':
            case '/':
            case 'b':
            case 'f':
            case 'n':
            case 'r':
            case 't':
                continue;
            case 'u':
                if (UNLIKELY(!isxdigit(next(lexer)))) {
                    goto error;
                }

                if (UNLIKELY(!isxdigit(next(lexer)))) {
                    goto error;
                }

                if (UNLIKELY(!isxdigit(next(lexer)))) {
                    goto error;
                }

                if (UNLIKELY(!isxdigit(next(lexer)))) {
                    goto error;
                }

                break;
            default:
                goto error;
            }
        }

        if (chr == '"') {
            backup(lexer);
            emit(lexer, JSON_TOK_STRING);

            next(lexer);
            ignore(lexer);

            return lexer_json;
        }
    }

error:
    return emit_end(lexer, JSON_TOK_ERROR);
}

static int accept_run(struct lexer *lexer, const char *run)
{
    for (; *run; run++) {
        if (UNLIKELY(next(lexer) != *run)) {
            return -EINVAL;
        }
    }

    return 0;
}

static void *lexer_boolean(struct lexer *lexer)
{
    /* Already matched either `t' or `f' at this point */

    switch (next(lexer)) {
    case 'r':
        if (LIKELY(!accept_run(lexer, "ue"))) {
            return emit_cont(lexer, JSON_TOK_TRUE);
        }
        break;
    case 'a':
        if (LIKELY(!accept_run(lexer, "lse"))) {
            return emit_cont(lexer, JSON_TOK_FALSE);
        }
        break;
    }

    return emit_end(lexer, JSON_TOK_ERROR);
}

static void *lexer_null(struct lexer *lexer)
{
    if (UNLIKELY(accept_run(lexer, "ull") < 0)) {
        return emit_end(lexer, JSON_TOK_ERROR);
    }

    return emit_cont(lexer, JSON_TOK_NULL);
}

static void *lexer_number(struct lexer *lexer)
{
    while (true) {
        int chr = next(lexer);

        if (isdigit(chr) || chr == '.') {
            continue;
        }

        backup(lexer);
        return emit_cont(lexer, JSON_TOK_NUMBER);
    }
}

static void *lexer_json(struct lexer *lexer)
{
    while (true) {
        int chr = next(lexer);

        switch (chr) {
        case '\0':
            return emit_end(lexer, JSON_TOK_EOF);

        case '}':
        case '{':
        case '[':
        case ']':
        case ',':
        case ':':
            return emit_cont(lexer, (enum json_tokens)chr);

        case '"':
            return lexer_string;

        case 'n':
            return lexer_null;

        case 't':
        case 'f':
            return lexer_boolean;

        case '-':
            if (LIKELY(isdigit(peek(lexer)))) {
                return lexer_number;
            }

            /* fallthrough */
        default:
            if (isspace(chr)) {
                ignore(lexer);
                continue;
            }

            if (LIKELY(isdigit(chr))) {
                return lexer_number;
            }

            return emit_end(lexer, JSON_TOK_ERROR);
        }
    }
}

static void lexer_init(struct lexer *lexer, char *data, size_t len)
{
    lexer->state = lexer_json;
    lexer->start = data;
    lexer->pos = data;
    lexer->end = data + len;
    lexer->token.type = JSON_TOK_NONE;
}

static int obj_init(struct json_obj *json, char *data, size_t len)
{
    struct token token;

    lexer_init(&json->lexer, data, len);

    if (UNLIKELY(!lexer_next(&json->lexer, &token))) {
        return -EINVAL;
    }

    if (UNLIKELY(token.type != JSON_TOK_OBJECT_START)) {
        return -EINVAL;
    }

    return 0;
}

static int element_token(enum json_tokens token)
{
    switch (token) {
    case JSON_TOK_OBJECT_START:
    case JSON_TOK_LIST_START:
    case JSON_TOK_STRING:
    case JSON_TOK_NUMBER:
    case JSON_TOK_TRUE:
    case JSON_TOK_FALSE:
        return 0;
    default:
        return -EINVAL;
    }
}

static int obj_next(struct json_obj *json, struct json_obj_key_value *kv)
{
    struct token token;

    if (UNLIKELY(!lexer_next(&json->lexer, &token))) {
        return -EINVAL;
    }

    /* Match end of object or next key */
    switch (token.type) {
    case JSON_TOK_OBJECT_END:
        kv->key = NULL;
        kv->key_len = 0;
        kv->value = token;

        return 0;
    case JSON_TOK_COMMA:
        if (UNLIKELY(!lexer_next(&json->lexer, &token))) {
            return -EINVAL;
        }

        if (UNLIKELY(token.type != JSON_TOK_STRING)) {
            return -EINVAL;
        }

        /* fallthrough */
    case JSON_TOK_STRING:
        kv->key = token.start;
        kv->key_len = (size_t)(token.end - token.start);
        break;
    default:
        return -EINVAL;
    }

    /* Match : after key */
    if (UNLIKELY(!lexer_next(&json->lexer, &token))) {
        return -EINVAL;
    }

    if (UNLIKELY(token.type != JSON_TOK_COLON)) {
        return -EINVAL;
    }

    /* Match value */
    if (UNLIKELY(!lexer_next(&json->lexer, &kv->value))) {
        return -EINVAL;
    }

    return element_token(kv->value.type);
}

static int arr_next(struct json_obj *json, struct token *value)
{
    if (UNLIKELY(!lexer_next(&json->lexer, value))) {
        return -EINVAL;
    }

    if (value->type == JSON_TOK_LIST_END) {
        return 0;
    }

    if (value->type == JSON_TOK_COMMA) {
        if (UNLIKELY(!lexer_next(&json->lexer, value))) {
            return -EINVAL;
        }
    }

    return element_token(value->type);
}

static int decode_num(const struct token *token, int32_t *num)
{
    /* FIXME: strtod() is not available in newlib/minimal libc,
     * so using strtol() here.
     */
    char *endptr;
    char prev_end;

    prev_end = *token->end;
    *token->end = '\0';

    errno = 0;
    long v = strtol(token->start, &endptr, 10);
    if ((long)(int)v != v) {
        return -ERANGE;
    }

    *num = (int)v;

    *token->end = prev_end;

    if (errno != 0) {
        return -errno;
    }

    if (endptr != token->end) {
        return -EINVAL;
    }

    return 0;
}

static bool equivalent_types(enum json_tokens type1, enum json_tokens type2)
{
    if (type1 == JSON_TOK_TRUE || type1 == JSON_TOK_FALSE) {
        return type2 == JSON_TOK_TRUE || type2 == JSON_TOK_FALSE;
    }

    return type1 == type2;
}

static int obj_parse(struct json_obj *obj,
                     const struct json_obj_descr *descr,
                     size_t descr_len,
                     void *val);
static int arr_parse(struct json_obj *obj,
                     const struct json_obj_descr *elem_descr,
                     size_t max_elements,
                     void *field,
                     void *val);

static int decode_value(struct json_obj *obj,
                        const struct json_obj_descr *descr,
                        struct token *value,
                        void *field,
                        void *val)
{

    if (!equivalent_types(value->type, descr->type)) {
        return -EINVAL;
    }

    switch (descr->type) {
    case JSON_TOK_OBJECT_START:
        return obj_parse(obj, descr->object.sub_descr,
                         descr->object.sub_descr_len, field);
    case JSON_TOK_LIST_START:
        return arr_parse(obj, descr->array.element_descr,
                         descr->array.n_elements, field, val);
    case JSON_TOK_FALSE:
    case JSON_TOK_TRUE: {
        bool *v = field;

        *v = value->type == JSON_TOK_TRUE;

        return 0;
    }
    case JSON_TOK_NUMBER: {
        int32_t *num = field;

        return decode_num(value, num);
    }
    case JSON_TOK_STRING: {
        char **str = field;

        *value->end = '\0';
        *str = value->start;

        return 0;
    }
    default:
        return -EINVAL;
    }
}

static ptrdiff_t get_elem_size(const struct json_obj_descr *descr)
{
    switch (descr->type) {
    case JSON_TOK_NUMBER:
        return sizeof(int32_t);
    case JSON_TOK_STRING:
        return sizeof(char *);
    case JSON_TOK_TRUE:
    case JSON_TOK_FALSE:
        return sizeof(bool);
    case JSON_TOK_LIST_START:
        return (ptrdiff_t)descr->array.n_elements *
               get_elem_size(descr->array.element_descr);
    case JSON_TOK_OBJECT_START: {
        ptrdiff_t total = 0;
        size_t i;

        for (i = 0; i < descr->object.sub_descr_len; i++) {
            ptrdiff_t s = get_elem_size(&descr->object.sub_descr[i]);

            total += (ptrdiff_t)ROUND_UP(s, descr->object.sub_descr[i].align);
        }

        return total;
    }
    default:
        return -EINVAL;
    }
}

static int arr_parse(struct json_obj *obj,
                     const struct json_obj_descr *elem_descr,
                     size_t max_elements,
                     void *field,
                     void *val)
{
    ptrdiff_t elem_size = get_elem_size(elem_descr);
    void *last_elem = (char *)field + elem_size * (ptrdiff_t)max_elements;
    size_t *elements = (size_t *)((char *)val + elem_descr->offset);
    struct token value;

    assert(elem_size > 0);

    *elements = 0;

    while (!arr_next(obj, &value)) {
        if (value.type == JSON_TOK_LIST_END) {
            return 0;
        }

        if (UNLIKELY(field == last_elem)) {
            return -ENOSPC;
        }

        if (UNLIKELY(decode_value(obj, elem_descr, &value, field, val) < 0)) {
            return -EINVAL;
        }

        (*elements)++;
        field = (char *)field + elem_size;
    }

    return -EINVAL;
}

static int obj_parse(struct json_obj *obj,
                     const struct json_obj_descr *descr,
                     size_t descr_len,
                     void *val)
{
    struct json_obj_key_value kv;
    int32_t decoded_fields = 0;
    size_t i;
    int ret;

    while (!obj_next(obj, &kv)) {
        if (kv.value.type == JSON_TOK_OBJECT_END) {
            return decoded_fields;
        }

        for (i = 0; i < descr_len; i++) {
            void *decode_field = (char *)val + descr[i].offset;

            /* Field has been decoded already, skip */
            if (decoded_fields & (1 << i)) {
                continue;
            }

            /* Check if it's the i-th field */
            if (kv.key_len != descr[i].field_name_len) {
                continue;
            }

            if (memcmp(kv.key, descr[i].field_name, descr[i].field_name_len)) {
                continue;
            }

            /* Store the decoded value */
            ret = decode_value(obj, &descr[i], &kv.value, decode_field, val);
            if (UNLIKELY(UNLIKELY(ret < 0))) {
                return ret;
            }

            decoded_fields |= 1 << i;
            break;
        }
    }

    return -EINVAL;
}

int json_obj_parse(char *payload,
                   size_t len,
                   const struct json_obj_descr *descr,
                   size_t descr_len,
                   void *val)
{
    struct json_obj obj;
    int ret;

    assert(descr_len < (sizeof(ret) * CHAR_BIT - 1));

    ret = obj_init(&obj, payload, len);
    if (UNLIKELY(ret < 0)) {
        return ret;
    }

    return obj_parse(&obj, descr, descr_len, val);
}

/*
 * Routines has_zero() and has_value() are from
 * https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
 */
static ALWAYS_INLINE uint64_t has_zero(uint64_t v)
{
    return (v - 0x0101010101010101UL) & ~v & 0x8080808080808080UL;
}

static ALWAYS_INLINE uint64_t has_value(uint64_t x, char n)
{
    return has_zero(x ^ (~0UL / 255 * (uint64_t)n));
}

static char escape_as(char chr)
{
    static const char escaped[] = {'"', '\\', 'b', 'f', 'n', 'r', 't', 't'};
    uint64_t mask = has_value(0x225c080c0a0d0909UL, chr);
    return mask == 0 ? 0 : escaped[__builtin_clzl(mask) / 8];
}

static int json_escape_internal(const char *str,
                                json_append_bytes_t append_bytes,
                                void *data)
{
    const char *cur;
    const char *unescaped;
    int ret = 0;

    for (cur = unescaped = str; *cur; cur++) {
        char escaped = escape_as(*cur);

        if (escaped) {
            char bytes[2] = {'\\', escaped};

            if (cur - unescaped) {
                ret |= append_bytes(unescaped, (size_t)(cur - unescaped), data);
                unescaped = cur + 1;
            }

            ret |= append_bytes(bytes, 2, data);
        }
    }

    if (cur - unescaped)
        ret |= append_bytes(unescaped, (size_t)(cur - unescaped), data);

    return ret;
}

size_t json_calc_escaped_len(const char *str, size_t len)
{
    size_t escaped_len = len;
    size_t pos;

    for (pos = 0; pos < len; pos++) {
        if (escape_as(str[pos])) {
            escaped_len++;
        }
    }

    return escaped_len;
}

ssize_t json_escape(char *str, size_t *len, size_t buf_size)
{
    char *next; /* Points after next character to escape. */
    char *dest; /* Points after next place to write escaped character. */
    size_t escaped_len = json_calc_escaped_len(str, *len);

    if (escaped_len == *len) {
        /*
         * If no escape is necessary, there is nothing to do.
         */
        return 0;
    }

    if (UNLIKELY(escaped_len >= buf_size)) {
        return -ENOMEM;
    }

    /*
     * By walking backwards in the buffer from the end positions
     * of both the original and escaped strings, we avoid using
     * extra space. Characters in the original string are
     * overwritten only after they have already been escaped.
     */
    str[escaped_len] = '\0';
    for (next = &str[*len], dest = &str[escaped_len]; next != str;) {
        char next_c = *(--next);
        char escape = escape_as(next_c);

        if (escape) {
            *(--dest) = escape;
            *(--dest) = '\\';
        } else {
            *(--dest) = next_c;
        }
    }
    *len = escaped_len;

    return 0;
}

static int encode(const struct json_obj_descr *descr,
                  const void *val,
                  json_append_bytes_t append_bytes,
                  void *data,
                  bool escape_key);

static int arr_encode(const struct json_obj_descr *elem_descr,
                      const void *field,
                      const void *val,
                      json_append_bytes_t append_bytes,
                      void *data,
                      bool escape_key)
{
    ptrdiff_t elem_size = get_elem_size(elem_descr);
    /*
     * NOTE: Since an element descriptor's offset isn't meaningful (array
     * elements occur at multiple offsets in `val'), we use its space in
     * elem_descr to store the offset to the field containing the number of
     * elements.
     */
    size_t n_elem = *(size_t *)((char *)val + elem_descr->offset);
    int ret = append_bytes("[", 1, data);

    if (LIKELY(n_elem)) {
        n_elem--;
        for (size_t i = 0; i < n_elem; i++) {
            /*
             * Though "field" points at the next element in the array which we
             * need to encode, the value in elem_descr->offset is actually the
             * offset of the length field in the "parent" struct containing the
             * array.
             *
             * To patch things up, we lie to encode() about where the field is
             * by exactly the amount it will offset it.  This is a size
             * optimization for struct json_obj_descr: the alternative is to
             * keep a separate field next to element_descr which is an offset to
             * the length field in the parent struct, but that would add a
             * size_t to every descriptor.
             */
            ret |= encode(elem_descr, (char *)field - elem_descr->offset,
                          append_bytes, data, escape_key);

            ret |= append_bytes(",", 1, data);

            field = (char *)field + elem_size;
        }

        ret |= encode(elem_descr, (char *)field - elem_descr->offset,
                      append_bytes, data, escape_key);
    }

    return ret | append_bytes("]", 1, data);
}

static int str_encode(const char **str,
                      json_append_bytes_t append_bytes,
                      void *data)
{
    int ret = append_bytes("\"", 1, data);

    ret |= json_escape_internal(*str, append_bytes, data);

    return ret | append_bytes("\"", 1, data);
}

static int
num_encode(const int32_t *num, json_append_bytes_t append_bytes, void *data)
{
    char buf[INT_TO_STR_BUFFER_SIZE];
    size_t len;
    char *as_string = int_to_string(*num, buf, &len);

    return append_bytes(as_string, len, data);
}

static int
bool_encode(const bool *value, json_append_bytes_t append_bytes, void *data)
{
    if (*value) {
        return append_bytes("true", 4, data);
    }

    return append_bytes("false", 5, data);
}

int json_arr_encode_full(const struct json_obj_descr *descr,
                         const void *val,
                         json_append_bytes_t append_bytes,
                         void *data,
                         bool escape_key)
{
    void *ptr = (char *)val + descr->offset;

    return arr_encode(descr->array.element_descr, ptr, val, append_bytes, data,
                      escape_key);
}

static int encode(const struct json_obj_descr *descr,
                  const void *val,
                  json_append_bytes_t append_bytes,
                  void *data,
                  bool escape_key)
{
    void *ptr = (char *)val + descr->offset;

    switch (descr->type) {
    case JSON_TOK_FALSE:
    case JSON_TOK_TRUE:
        return bool_encode(ptr, append_bytes, data);
    case JSON_TOK_STRING:
        return str_encode(ptr, append_bytes, data);
    case JSON_TOK_LIST_START:
        return arr_encode(descr->array.element_descr, ptr, val,
                          append_bytes, data, escape_key);
    case JSON_TOK_OBJECT_START:
        return json_obj_encode_full(descr->object.sub_descr,
                                    descr->object.sub_descr_len, ptr, append_bytes,
                                    data, escape_key);
    case JSON_TOK_NUMBER:
        return num_encode(ptr, append_bytes, data);
    default:
        return -EINVAL;
    }
}

static inline int encode_key(const struct json_obj_descr *descr,
                             json_append_bytes_t append_bytes,
                             void *data,
                             bool escape_key)
{
    int ret;

    if (!escape_key) {
        /* Keys are encoded twice in the descriptor; once without quotes and
         * the trailing comma, and one with.  Doing it like so cuts some
         * indirect calls to append_bytes(), which in turn also potentially
         * cuts some branches in most implementations of it.  */
        ret = append_bytes(descr->field_name + descr->field_name_len,
                           descr->field_name_len + 3 /* 3=len('"":') */, data);
    } else {
        ret = str_encode((const char **)&descr->field_name, append_bytes, data);
        ret |= append_bytes(":", 1, data);
    }

    return ret;
}

int json_obj_encode_full(const struct json_obj_descr *descr,
                         size_t descr_len,
                         const void *val,
                         json_append_bytes_t append_bytes,
                         void *data,
                         bool escape_key)
{
    int ret = append_bytes("{", 1, data);

    if (LIKELY(descr_len)) {
        /* To avoid checking if we're encoding the last element on each
         * iteration of this loop, start at the second descriptor, and always
         * write the comma. Then, after the loop, encode the first descriptor.
         * If the descriptor array has only 1 element, this loop won't run. This
         * is fine since order isn't important for objects, and we save some
         * branches.  */

        for (size_t i = 1; i < descr_len; i++) {
            ret |= encode_key(&descr[i], append_bytes, data, escape_key);
            ret |= encode(&descr[i], val, append_bytes, data, escape_key);
            ret |= append_bytes(",", 1, data);
        }

        ret |= encode_key(&descr[0], append_bytes, data, escape_key);
        ret |= encode(&descr[0], val, append_bytes, data, escape_key);
    }

    return ret | append_bytes("}", 1, data);
}

struct appender {
    char *buffer;
    size_t used;
    size_t size;
};

static int append_bytes_to_buf(const char *bytes, size_t len, void *data)
{
    struct appender *appender = data;

    if (UNLIKELY(len > appender->size - appender->used)) {
        return -ENOMEM;
    }

    memcpy(appender->buffer + appender->used, bytes, len);
    appender->used += len;
    appender->buffer[appender->used] = '\0';

    return 0;
}

int json_obj_encode_buf(const struct json_obj_descr *descr,
                        size_t descr_len,
                        const void *val,
                        char *buffer,
                        size_t buf_size)
{
    struct appender appender = {.buffer = buffer, .size = buf_size};

    return json_obj_encode(descr, descr_len, val, append_bytes_to_buf,
                           &appender);
}

static int
measure_bytes(const char *bytes __attribute__((unused)), size_t len, void *data)
{
    ssize_t *total = data;

    *total += (ssize_t)len;

    return 0;
}

ssize_t json_calc_encoded_len(const struct json_obj_descr *descr,
                              size_t descr_len,
                              const void *val)
{
    ssize_t total = 0;
    int ret;

    ret = json_obj_encode(descr, descr_len, val, measure_bytes, &total);
    if (UNLIKELY(ret < 0)) {
        return ret;
    }

    return total;
}
