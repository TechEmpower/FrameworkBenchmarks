// ============================================================================
// gwdbg.h: helpers to let crash reports list G-WAN API calls in C scripts
//          (this file has been generated automatically, do not edit it)
// ----------------------------------------------------------------------------
#ifndef _GWDBG_H
#define _GWDBG_H

#include <stdarg.h>
#include <stddef.h>
#include <time.h>

typedef   signed char       s8;
typedef unsigned char       u8;
typedef   signed short int s16;
typedef unsigned short int u16;
typedef   signed int       s32;
typedef unsigned int       u32;

typedef unsigned long      ulong;

#if __WORDSIZE == 64
typedef          long int s64;
typedef long unsigned int u64;
#else
typedef          long long int s64;
typedef long long unsigned int u64;
#endif

#define xbuf_t   void
#define rgb_t    void
#define jsn_t    void
#define strtab_t void
#define prnd_t   void
#define rnd_t    void
#define md5_t    void
#define sha1_t   void
#define sha2_t   void
#define aes_t    void
#define tm_t     void 
#define bmp_t    void
#define kv_t     void
#define kv_item  void

typedef void(*kv_delfn_t)(void *value);
typedef void(*kv_recfn_t)(void *value);
typedef int(*kv_proc_t)(const kv_item *item, const void *user_defined_ctx);

typedef int (*make_data_t)(char *argv[]);
typedef int (*push_data_t)(char *argv[], xbuf_t *reply);
typedef void(*free_data_t)(char *argv[]);

extern void xbuf_frfile(xbuf_t *ctx, char *szFile)
{
   void(*p_xbuf_frfile)(xbuf_t *ctx, char *szFile) =
   (void(*)(xbuf_t *ctx, char *szFile))
   _xbuf_frfile;
   p_xbuf_frfile(ctx, szFile);
}

extern int xbuf_tofile(xbuf_t *ctx, char *szFile)
{
   int(*p_xbuf_tofile)(xbuf_t *ctx, char *szFile) =
   (int(*)(xbuf_t *ctx, char *szFile))
   _xbuf_tofile;
   return p_xbuf_tofile(ctx, szFile);
}

extern u32 xbuf_growto(xbuf_t *ctx, u32 len)
{
   u32(*p_xbuf_growto)(xbuf_t *ctx, u32 len) =
   (u32(*)(xbuf_t *ctx, u32 len))
   _xbuf_growto;
   return p_xbuf_growto(ctx, len);
}

extern void xbuf_empty(xbuf_t *ctx)
{
   void(*p_xbuf_empty)(xbuf_t *ctx) =
   (void(*)(xbuf_t *ctx))
   _xbuf_empty;
   p_xbuf_empty(ctx);
}

extern char *xbuf_getend(xbuf_t *ctx)
{
   char*(*p_xbuf_getend)(xbuf_t *ctx) =
   (char*(*)(xbuf_t *ctx))
   _xbuf_getend;
   return p_xbuf_getend(ctx);
}

extern void xbuf_attach(xbuf_t *ctx, char *ptr, s32 size, s32 len)
{
   void(*p_xbuf_attach)(xbuf_t *ctx, char *ptr, s32 size, s32 len) =
   (void(*)(xbuf_t *ctx, char *ptr, s32 size, s32 len))
   _xbuf_attach;
   p_xbuf_attach(ctx, ptr, size, len);
}

extern char *xbuf_detach(xbuf_t *ctx)
{
   char*(*p_xbuf_detach)(xbuf_t *ctx) =
   (char*(*)(xbuf_t *ctx))
   _xbuf_detach;
   return p_xbuf_detach(ctx);
}

extern void xbuf_free(xbuf_t *ctx)
{
   void(*p_xbuf_free)(xbuf_t *ctx) =
   (void(*)(xbuf_t *ctx))
   _xbuf_free;
   p_xbuf_free(ctx);
}

extern void xbuf_clear(xbuf_t *ctx)
{
   void(*p_xbuf_clear)(xbuf_t *ctx) =
   (void(*)(xbuf_t *ctx))
   _xbuf_clear;
   p_xbuf_clear(ctx);
}

extern void xbuf_init(xbuf_t *ctx)
{
   void(*p_xbuf_init)(xbuf_t *ctx) =
   (void(*)(xbuf_t *ctx))
   _xbuf_init;
   p_xbuf_init(ctx);
}

extern void xbuf_reset(xbuf_t *ctx)
{
   void(*p_xbuf_reset)(xbuf_t *ctx) =
   (void(*)(xbuf_t *ctx))
   _xbuf_reset;
   p_xbuf_reset(ctx);
}

extern char *xbuf_ncat(xbuf_t *ctx, char *src, s32 srclen)
{
   char*(*p_xbuf_ncat)(xbuf_t *ctx, char *src, s32 srclen) =
   (char*(*)(xbuf_t *ctx, char *src, s32 srclen))
   _xbuf_ncat;
   return p_xbuf_ncat(ctx, src, srclen);
}

extern char *xbuf_cat(xbuf_t *ctx, char *str)
{
   char*(*p_xbuf_cat)(xbuf_t *ctx, char *str) =
   (char*(*)(xbuf_t *ctx, char *str))
   _xbuf_cat;
   return p_xbuf_cat(ctx, str);
}

extern char *xbuf_vxcat(xbuf_t *ctx, char *fmt, va_list a)
{
   char*(*p_xbuf_vxcat)(xbuf_t *ctx, char *fmt, va_list a) =
   (char*(*)(xbuf_t *ctx, char *fmt, va_list a))
   _xbuf_vxcat;
   return p_xbuf_vxcat(ctx, fmt, a);
}

extern char *xbuf_xcat(xbuf_t *ctx, char *fmt, ...)
{
   va_list a;
   va_start(a, fmt);
   char *ret = xbuf_vxcat(ctx, fmt, a);
   va_end(a);
   return ret;
}

extern char *xbuf_vcat(xbuf_t *ctx, const strtab_t *array, int nbr)
{
   char*(*p_xbuf_vcat)(xbuf_t *ctx, const strtab_t *array, int nbr) =
   (char*(*)(xbuf_t *ctx, const strtab_t *array, int nbr))
   _xbuf_vcat;
   return p_xbuf_vcat(ctx, array, nbr);
}

extern void xbuf_sort(xbuf_t *ctx, char separator, s32 remove_duplicates)
{
   void(*p_xbuf_sort)(xbuf_t *ctx, char separator, s32 remove_duplicates) =
   (void(*)(xbuf_t *ctx, char separator, s32 remove_duplicates))
   _xbuf_sort;
   p_xbuf_sort(ctx, separator, remove_duplicates);
}

extern char *xbuf_findstr(xbuf_t *ctx, char *str)
{
   char *(*p_xbuf_findstr)(xbuf_t *ctx, char *str) =
   (char *(*)(xbuf_t *ctx, char *str))
   _xbuf_findstr;
   return p_xbuf_findstr(ctx, str);
}

extern char *xbuf_repl(xbuf_t *ctx, char *oldstr, char *newstr)
{
   char *(*p_xbuf_repl)(xbuf_t *ctx, char *oldstr, char *newstr) =
   (char *(*)(xbuf_t *ctx, char *oldstr, char *newstr))
   _xbuf_repl;
   return p_xbuf_repl(ctx, oldstr, newstr);
}

extern char *xbuf_replfrto(xbuf_t *ctx, char *beg, char *end, char *oldstr,
       char *newstr)
{
   char *(*p_xbuf_replfrto)(xbuf_t *ctx, char *beg, char *end, char *oldstr, char *newstr) =
   (char *(*)(xbuf_t *ctx, char *beg, char *end, char *old, char *newstr))
   _xbuf_replfrto;
   return p_xbuf_replfrto(ctx, beg, end, oldstr, newstr);
}

extern void xbuf_truncptr(xbuf_t *ctx, char *ptr)
{
   void(*p_xbuf_truncptr)(xbuf_t *ctx, char *ptr) =
   (void(*)(xbuf_t *ctx, char *ptr))
   _xbuf_truncptr;
   p_xbuf_truncptr(ctx, ptr);
}

extern void xbuf_trunclen(xbuf_t *ctx, s32 len)
{
   void(*p_xbuf_trunclen)(xbuf_t *ctx, s32 len) =
   (void(*)(xbuf_t *ctx, s32 len))
   _xbuf_trunclen;
   p_xbuf_trunclen(ctx, len);
}

extern s32 xbuf_getln(xbuf_t *ctx, char *dst, s32 dstlen)
{
   s32(*p_xbuf_getln)(xbuf_t *ctx, char *dst, s32 dstlen) =
   (s32(*)(xbuf_t *ctx, char *dst, s32 dstlen))
   _xbuf_getln;
   return p_xbuf_getln(ctx, dst, dstlen);
}

extern s32 xbuf_pull(xbuf_t *ctx, char *dst, s32 dstlen)
{
   s32(*p_xbuf_pull)(xbuf_t *ctx, char *dst, s32 dstlen) =
   (s32(*)(xbuf_t *ctx, char *dst, s32 dstlen))
   _xbuf_pull;
   return p_xbuf_pull(ctx, dst, dstlen);
}

extern void xbuf_delete(xbuf_t *ctx, char *pos, s32 len, char *bytes)
{
   void(*p_xbuf_delete)(xbuf_t *ctx, char *pos, s32 len, char *bytes) =
   (void(*)(xbuf_t *ctx, char *pos, s32 len, char *bytes))
   _xbuf_delete;
   p_xbuf_delete(ctx, pos, len, bytes);
}

extern s32 xbuf_insert(xbuf_t *ctx, char *pos, s32 len, char *bytes)
{
   s32(*p_xbuf_insert)(xbuf_t *ctx, char *pos, s32 len, char *bytes) =
   (s32(*)(xbuf_t *ctx, char *pos, s32 len, char *bytes))
   _xbuf_insert;
   return p_xbuf_insert(ctx, pos, len, bytes);
}

extern s32 xbuf_frurl(xbuf_t *ctx, char *host, u32 port, u32 method, char *uri,
         u32 mstimeout, char *headers)
{
   s32(*p_xbuf_frurl)(xbuf_t *ctx, char *host, u32 port, u32 method,
            char *uri, u32 mstimeout, char *headers) =
   (s32(*)(xbuf_t *ctx, char *host, u32 port, u32 method,
            char *uri, u32 mstimeout, char *headers))
   _xbuf_frurl;
   return p_xbuf_frurl(ctx, host, port, method, uri, mstimeout, headers);
}

extern u32 cycles(void)
{
   u32(*p_cycles)(void) =
   (u32(*)(void))
   _cycles;
   return p_cycles();
}

extern u64 cycles64(void)
{
   u64(*p_cycles64)(void) =
   (u64(*)(void))
   _cycles64;
   return p_cycles64();
}

extern u64 getns(void)
{
   u64(*p_getns)(void) =
   (u64(*)(void))
   _getns;
   return p_getns();
}

extern u64 getus(void)
{
   u64(*p_getus)(void) =
   (u64(*)(void))
   _getus;
   return p_getus();
}

extern u64 getms(void)
{
   u64(*p_getms)(void) =
   (u64(*)(void))
   _getms;
   return p_getms();
}

extern int fw_block(char *addr, int is_ip)
{
   int(*p_fw_block)(char *addr, int is_ip) =
   (int(*)(char *addr, int is_ip))
   _fw_block;
   p_fw_block(addr, is_ip);
}

extern int fw_allow(char *addr, int is_ip)
{
   int(*p_fw_allow)(char *addr, int is_ip) =
   (int(*)(char *addr, int is_ip))
   _fw_allow;
   p_fw_allow(addr, is_ip);
}

extern void fw_state(xbuf_t *rules)
{
   void(*p_fw_state)(xbuf_t *rules) =
   (void(*)(xbuf_t *rules))
   _fw_state;
   p_fw_state(rules);
}

extern void sw_init(prnd_t *rnd, u32 seed)
{
   void(*p_sw_init)(prnd_t *rnd, u32 seed) =
   (void(*)(prnd_t *rnd, u32 seed))
   _sw_init;
   p_sw_init(rnd, seed);
}

extern u32 sw_rand(prnd_t *rnd)
{
   u32(*p_sw_rand)(prnd_t *rnd) =
   (u32(*)(prnd_t *rnd))
   _sw_rand;
   return p_sw_rand(rnd);
}

extern void hw_init(rnd_t *rnd)
{
   void(*p_hw_init)(rnd_t *rnd) =
   (void(*)(rnd_t *rnd))
   _hw_init;
   p_hw_init(rnd);
}

extern u32 hw_rand(rnd_t *rnd)
{
   u32(*p_hw_rand)(rnd_t *rnd) =
   (u32(*)(rnd_t *rnd))
   _hw_rand;
   return p_hw_rand(rnd);
}

extern u32 crc_32(char *data, u32 len, u32 crc)
{
   u32(*p_crc_32)(char *data, u32 len, u32 crc) =
   (u32(*)(char *data, u32 len, u32 crc))
   _crc_32;
   return p_crc_32(data, len, crc);
}

extern u32 adler_32(char *data, u32 len, u32 crc)
{
   u32(*p_adler_32)(char *data, u32 len, u32 crc) =
   (u32(*)(char *data, u32 len, u32 crc))
   _adler_32;
   return p_adler_32(data, len, crc);
}

extern void md5_init(md5_t *ctx)
{
   void(*p_md5_init)(md5_t *ctx) =
   (void(*)(md5_t *ctx))
   _md5_init;
   p_md5_init(ctx);
}

extern void md5_add(md5_t *ctx, u8 *src, int srclen)
{
   void(*p_md5_add)(md5_t *ctx, u8 *src, int srclen) =
   (void(*)(md5_t *ctx, u8 *src, int srclen))
   _md5_add;
   p_md5_add(ctx, src, srclen);
}

extern void md5_end(md5_t *ctx, u8 *dst)
{
   void(*p_md5_end)(md5_t *ctx, u8 *dst) =
   (void(*)(md5_t *ctx, u8 *dst))
   _md5_end;
   p_md5_end(ctx, dst);
}

extern void md5(u8 *input, int ilen, u8 *dst)
{
   void(*p_md5)(u8 *input, int ilen, u8 *dst) =
   (void(*)(u8 *input, int ilen, u8 *dst))
   _md5;
   p_md5(input, ilen, dst);
}

extern void sha1_init(sha1_t *ctx)
{
   void(*p_sha1_init)(sha1_t *ctx) =
   (void(*)(sha1_t *ctx))
   _sha1_init;
   p_sha1_init(ctx);
}

extern void sha1_add(sha1_t *ctx, u8 *src, int srclen)
{
   void(*p_sha1_add)(sha1_t *ctx, u8 *src, int srclen) =
   (void(*)(sha1_t *ctx, u8 *src, int srclen))
   _sha1_add;
   p_sha1_add(ctx, src, srclen);
}

extern void sha1_end(sha1_t *ctx, u8 *dst)
{
   void(*p_sha1_end)(sha1_t *ctx, u8 *dst) =
   (void(*)(sha1_t *ctx, u8 *dst))
   _sha1_end;
   p_sha1_end(ctx, dst);
}

extern void sha1(u8 *input, int ilen, u8 *dst)
{
   void(*p_sha1)(u8 *input, int ilen, u8 *dst) =
   (void(*)(u8 *input, int ilen, u8 *dst))
   _sha1;
   p_sha1(input, ilen, dst);
}

extern void sha2_init(sha2_t *ctx)
{
   void(*p_sha2_init)(sha2_t *ctx) =
   (void(*)(sha2_t *ctx))
   _sha2_init;
   p_sha2_init(ctx);
}

extern void sha2_add(sha2_t *ctx, u8 *src, int srclen)
{
   void(*p_sha2_add)(sha2_t *ctx, u8 *src, int srclen) =
   (void(*)(sha2_t *ctx, u8 *src, int srclen))
   _sha2_add;
   p_sha2_add(ctx, src, srclen);
}

extern void sha2_end(sha2_t *ctx, u8 *dst)
{
   void(*p_sha2_end)(sha2_t *ctx, u8 *dst) =
   (void(*)(sha2_t *ctx, u8 *dst))
   _sha2_end;
   p_sha2_end(ctx, dst);
}

extern void sha2(u8 *input, int ilen, u8 *dst)
{
   void(*p_sha2)(u8 *input, int ilen, u8 *dst) =
   (void(*)(u8 *input, int ilen, u8 *dst))
   _sha2;
   p_sha2(input, ilen, dst);
}

extern void aes_init(aes_t *ctx, u32 mode, u8 *key, u32 keylen)
{
   void(*p_aes_init)(aes_t *ctx, u32 mode, u8 *key, u32 keylen) =
   (void(*)(aes_t *ctx, u32 mode, u8 *key, u32 keylen))
   _aes_init;
   p_aes_init(ctx, mode, key, keylen);
}

extern void aes_enc(aes_t *ctx, u32 mode, u32 len, u8 *iv, u8 *src, u8 *dst)
{
   void(*p_aes_enc)(aes_t *ctx, u32 mode, u32 len, u8 *iv, u8 *src, u8 *dst) =
   (void(*)(aes_t *ctx, u32 mode, u32 len, u8 *iv, u8 *src, u8 *dst))
   _aes_enc;
   p_aes_enc(ctx, mode, len, iv, src, dst);
}

// p_qlz_cmp     = _qlz_cmp,

// p_qlz_exp     = _qlz_exp,

extern u32 zlib_cmp(char *src, u32 *crc, u32 srclen, char *dst, u32 dstlen,
         int gzip)
{
   u32(*p_zlib_cmp)(char *src, u32 *crc, u32 srclen, char *dst,
            u32 dstlen, int gzip) =
   (u32(*)(char *src, u32 *crc, u32 srclen, char *dst,
            u32 dstlen, int gzip))
   _zlib_cmp;
   return p_zlib_cmp(src, crc, srclen, dst, dstlen, gzip);
}

//p_gz_exp       = _gz_exp,

extern void get_arg(char *name, char **value, int argc, char *argv[])
{
   void(*p_get_arg)(char *name, char **value, int argc, char *argv[]) =
   (void(*)(char *name, char **value, int argc, char *argv[]))
   _get_arg;
   p_get_arg(name, value, argc, argv);
}

extern xbuf_t *get_reply(char *argv[])
{
   xbuf_t*(*p_get_reply)(char *argv[]) =
   (xbuf_t*(*)(char *argv[]))
   _get_reply;
   return p_get_reply(argv);
}

extern void set_reply(char *argv[], char *buf, u32 len, u32 status)
{
   void(*p_set_reply)(char *argv[], char *buf, u32 len, u32 status) =
   (void(*)(char *argv[], char *buf, u32 len, u32 status))
   _set_reply;
   p_set_reply(argv, buf, len, status);
}

extern void wake_up(char *argv[], int ms_or_fd, int type)
{
   void(*p_wake_up)(char *argv[], int ms_or_fd, int type) =
   (void(*)(char *argv[], int ms_or_fd, int type))
   _wake_up;
   p_wake_up(argv, ms_or_fd, type);
}

extern u64 get_env(char *argv[], int name)
{
   u64(*p_get_env)(char *argv[], int name) =
   (u64(*)(char *argv[], int name))
   _get_env;
   return p_get_env(argv, name);
}

extern char *http_status(int code)
{
   char*(*p_http_status)(int code) =
   (char*(*)(int code))
   _http_status;
   return p_http_status(code);
}

extern char *http_error(int code)
{
   char*(*p_http_error)(int code) =
   (char*(*)(int code))
   _http_error;
   return p_http_error(code);
}

extern void http_header(u32 flags, char *buf, u32 buflen, char *argv[])
{
   void(*p_http_header)(u32 flags, char *buf, u32 buflen, char *argv[]) =
   (void(*)(u32 flags, char *buf, u32 buflen, char *argv[]))
   _http_header;
   p_http_header(flags, buf, buflen, argv);
}

extern void build_vheaders(char *argv[], char *fmt, va_list a)
{
   void(*p_build_vheaders)(char *argv[], char *fmt, va_list a) =
   (void(*)(char *argv[], char *fmt, va_list a))
   _build_vheaders;
   p_build_vheaders(argv, fmt, a);
}

extern void build_headers(char *argv[], char *fmt, ...)
{
   va_list a;
   va_start(a, fmt);
   build_vheaders(argv, fmt, a);
   va_end(a);
}

extern void log_err(char *argv[], const char *msg)
{
   void(*p_log_err)(char *argv[], const char *msg) =
   (void(*)(char *argv[], const char *msg))
   _log_err;
   p_log_err(argv, msg);
}

extern long cacheadd(char *argv[], char *file, char *buf, u32 buflen, 
                     char *mime, u32 code, u32 expire)
{
   long(*p_cacheadd)(char *argv[], char *file, char *buf, u32 buflen,
                     char *mime, u32 code, u32 expire) =
   (long(*)(char *argv[], char *file, char *buf, u32 buflen, char *mime,
            u32 code, u32 expire))
   _cacheadd;
   return p_cacheadd(argv, file, buf, buflen, mime, code, expire);
}

extern void cachedel(char *argv[], char *file)
{
   void(*p_cachedel)(char *argv[], char *file) =
   (void(*)(char *argv[], char *file))
   _cachedel;
   p_cachedel(argv, file);
}

extern char *cacheget(char *argv[], char *uri, u32 *buflen, char **mime, 
                      u32 *code, u32 *modified, u32 *expire)
{
   char*(*p_cacheget)(char *argv[], char *uri, u32 *buflen, char **mime, 
          u32 *code, u32 *modified, u32 *expire) =
   (char*(*)(char *argv[], char *uri, u32 *buflen, char **mime, u32 *code,
            u32 *modified, u32 *expire))
   _cacheget;
   return p_cacheget(argv, uri, buflen, mime, code, modified, expire);
}

extern int push_list_add(char *argv[], char *feed_name,
              make_data_t make_fn, u32 make_freq, push_data_t push_fn, 
              u32 push_freq, free_data_t free_fn)
{
   int (*p_push_list_add)(char *argv[], char *feed_name, make_data_t make_fn, 
           u32 make_freq, push_data_t push_fn, u32 push_freq, 
           free_data_t free_fn) = 
   (int (*)(char *argv[], char *feed_name, make_data_t make_fn, 
           u32 make_freq, push_data_t push_fn, u32 push_freq, 
           free_data_t free_fn))
   _push_list_add;
   return p_push_list_add(argv, feed_name, make_fn, make_freq, push_fn, 
                          push_freq, free_fn);
}

extern u32 url_encode(u8 *dst, u8 *src, u32 maxdstlen)
{
   u32(*p_url_encode)(u8 *dst, u8 *src, u32 maxdstlen) =
   (u32(*)(u8 *dst, u8 *src, u32 maxdstlen))
   _url_encode;
   return p_url_encode(dst, src, maxdstlen);
}

extern u32 escape_html(u8 *dst, u8 *src, u32 maxdstlen)
{
   u32(*p_escape_html)(u8 *dst, u8 *src, u32 maxdstlen) =
   (u32(*)(u8 *dst, u8 *src, u32 maxdstlen))
   _escape_html;
   return p_escape_html(dst, src, maxdstlen);
}

extern u32 unescape_html(u8 *str)
{
   u32(*p_unescape_html)(u8 *str) =
   (u32(*)(u8 *str))
   _unescape_html;
   return p_unescape_html(str);
}

extern int html2txt(u8 *html, u8 *text, int maxtxlen)
{
   int(*p_html2txt)(u8 *html, u8 *text, int maxtxlen) =
   (int(*)(u8 *html, u8 *text, int maxtxlen))
   _html2txt;
   return p_html2txt(html, text, maxtxlen);
}

extern void throttle_reply(char *argv[], u16 kbps1, u16 kbps2, int global)
{
   void(*p_throttle_reply)(char *argv[], u16 kbps1, u16 kbps2, int global) =
   (void(*)(char *argv[], u16 kbps1, u16 kbps2, int global))
   _throttle_reply;
   return p_throttle_reply(argv, kbps1, kbps2, global);
}

extern int s_vsnprintf(char *str, size_t len, const char *fmt, va_list a)
{
   int(*p_s_vsnprintf)(char *str, size_t len, const char *fmt, va_list a) =
   (int(*)(char *str, size_t len, const char *fmt, va_list a))
   _s_vsnprintf;
   return p_s_vsnprintf(str, len, fmt, a);
}

extern int s_snprintf(char *str, size_t len, const char *fmt, ...)
{
   va_list a;
   va_start(a, fmt);
   int ret = s_vsnprintf(str, len, fmt, a);
   va_end(a);
   return ret;
}

extern int gif_build(u8 *gif, u8 *bitmap, u32 width, u32 height, u8 *palette,
         u32 nbcolors, int transparency, u8 *comment)
{
   int(*p_gif_build)(u8 *gif, u8 *bitmap, u32 width, u32 height,
            u8 *palette, u32 nbcolors, int transparency, u8 *comment) =
   (int(*)(u8 *gif, u8 *bitmap, u32 width, u32 height,
            u8 *palette, u32 nbcolors, int transparency, u8 *comment))
   _gif_build;
   return p_gif_build(gif, bitmap, width, height, palette, nbcolors,
                      transparency, comment);
}

extern u8 *gif_parse(u8 *buf, u32 buflen, u32 *width, u32 *height, u8 *palette,
         u32 *nbcolors, int *transparent, u8 **comment)
{
   u8*(*p_gif_parse)(u8 *buf, u32 buflen, u32 *width, u32 *height,
            u8 *palette, u32 *nbcolors, int *transparent, u8 **comment) =
   (u8*(*)(u8 *buf, u32 buflen, u32 *width, u32 *height,
            u8 *palette, u32 *nbcolors, int *transparent, u8 **comment))
   _gif_parse;
   return p_gif_parse(buf, buflen, width, height, palette, nbcolors,
                      transparent, comment);
}

extern time_t s_time(void)
{
   time_t(*p_s_time)(void) =
   (time_t(*)(void))
   _s_time;
   return p_s_time();
}

extern tm_t *s_gmtime(time_t t, tm_t *ts)
{
   tm_t*(*p_s_gmtime)(time_t t, tm_t *ts) =
   (tm_t*(*)(time_t t, tm_t *ts))
   _s_gmtime;
   return p_s_gmtime(t, ts);
}

extern tm_t *s_localtime(time_t t, tm_t *ts)
{
   tm_t*(*p_s_localtime)(time_t t, tm_t *ts) =
   (tm_t*(*)(time_t t, tm_t *ts))
   _s_localtime;
   return p_s_localtime(t, ts);
}

extern char *s_asctime(time_t t, char *buf)
{
   char*(*p_s_asctime)(time_t t, char *buf) =
   (char*(*)(time_t t, char *buf))
   _s_asctime;
   return p_s_asctime(t, buf);
}

extern size_t rfc2time(char *s)
{
   size_t(*p_rfc2time)(char *s) =
   (size_t(*)(char *s))
   _rfc2time;
   return p_rfc2time(s);
}

extern char *time2rfc(time_t t, char *buf)
{
   char*(*p_time2rfc)(time_t t, char *buf) =
   (char*(*)(time_t t, char *buf))
   _time2rfc;
   return p_time2rfc(t, buf);
}

extern void dr_gradient(u8 *palette, int nbcolors, rgb_t *steps, int nbsteps)
{
   void(*p_dr_gradient)(u8 *palette, int nbcolors, rgb_t *steps, int nbsteps) =
   (void(*)(u8 *palette, int nbcolors, rgb_t *steps, int nbsteps))
   _dr_gradient;
   p_dr_gradient(palette, nbcolors, steps, nbsteps);
}

extern u32 dr_vtext(bmp_t *img, u8 *font, const char *fmt, va_list a)
{
   u32(*p_dr_vtext)(bmp_t *img, u8 *font, const char *fmt, va_list a) =
   (u32(*)(bmp_t *img, u8 *font, const char *fmt, va_list a))
   _dr_vtext;
   return p_dr_vtext(img, font, fmt, a);
}

extern u32 dr_text(bmp_t *img, u8 *font, const char *fmt, ...)
{
   va_list a;
   va_start(a, fmt);
   u32 ret = dr_vtext(img, font, fmt, a);
   va_end(a);
   return ret;
}

extern void dr_line(bmp_t *img, int x1, int y1, int x2, int y2)
{
   void(*p_dr_line)(bmp_t *img, int x1, int y1, int x2, int y2) =
   (void(*)(bmp_t *img, int x1, int y1, int x2, int y2))
   _dr_line;
   p_dr_line(img, x1, y1, x2, y2);
}

extern void dr_circle(bmp_t *img, int x, int y, int radius)
{
   void(*p_dr_circle)(bmp_t *img, int x, int y, int radius) =
   (void(*)(bmp_t *img, int x, int y, int radius))
   _dr_circle;
   p_dr_circle(img, x, y, radius);
}

extern void dr_chart(bmp_t *img, u8 *title, u8 *subtitle, u8 **tags, u32 ntag,
         float *val, u32 nval)
{
   void(*p_dr_chart)(bmp_t *img, u8 *title, u8 *subtitle, u8 **tags,
            u32 ntag, float *val, u32 nval) =
   (void(*)(bmp_t *img, u8 *title, u8 *subtitle, u8 **tags, u32 ntag,
            float *val, u32 nval))
   _dr_chart;
   p_dr_chart(img, title, subtitle, tags, ntag, val, nval);
}

extern int sendemail(char *mail_server, char *src_mailaddr, char *dst_mailaddr,
         char *subject, char *text, char *auth_user, char *auth_pass,
         char *error)
{
   int(*p_sendemail)(char *mail_server, char *src_mailaddr,
            char *dst_mailaddr, char *subject, char *text, char *auth_user,
            char *auth_pass, char *error) =
   (int(*)(char *mail_server, char *src_mailaddr,
            char *dst_mailaddr, char *subject, char *text, char *auth_user,
            char *auth_pass, char *error))
   _sendemail;
   return p_sendemail(mail_server, src_mailaddr, dst_mailaddr, subject, text,
                      auth_user, auth_pass, error);
}

extern int isvalidemailaddr(char *szEmail)
{
   int(*p_isvalidemailaddr)(char *szEmail) =
   (int(*)(char *szEmail))
   _isvalidemailaddr;
   return p_isvalidemailaddr(szEmail);
}

extern void kv_init(kv_t *store, char *name, int max_nbr_items, u32 flags,
         kv_delfn_t delfn, kv_recfn_t recfn)
{
   void(*p_kv_init)(kv_t *store, char *name, int max_nbr_items,
            u32 flags, kv_delfn_t delfn, kv_recfn_t recfn) =
   (void(*)(kv_t *store, char *name, int max_nbr_items,
            u32 flags, kv_delfn_t delfn, kv_recfn_t recfn))
   _kv_init;
   p_kv_init(store, name, max_nbr_items, flags, delfn, recfn);
}

extern kv_item *kv_add(kv_t *store, kv_item *item)
{
   kv_item*(*p_kv_add)(kv_t *store, kv_item *item) =
   (kv_item*(*)(kv_t *store, kv_item *item))
   _kv_add;
   return p_kv_add(store, item);
}

extern char *kv_get(kv_t *store, const char *key, int klen)
{
   char*(*p_kv_get)(kv_t *store, const char *key, int klen) =
   (char*(*)(kv_t *store, const char *key, int klen))
   _kv_get;
   return p_kv_get(store, key, klen);
}

extern int kv_del(kv_t *store, const char *key, int klen)
{
   int(*p_kv_del)(kv_t *store, const char *key, int klen) =
   (int(*)(kv_t *store, const char *key, int klen))
   _kv_del;
   return p_kv_del(store, key, klen);
}

extern void kv_free(kv_t *store)
{
   void(*p_kv_free)(kv_t *store) =
   (void(*)(kv_t *store))
   _kv_free;
   p_kv_free(store);
}

extern int kv_do(kv_t *store, const char *key, int klen, kv_proc_t kv_proc,
         void *user_defined_ctx)
{
   int(*p_kv_do)(kv_t *store, const char *key, int klen,
            kv_proc_t kv_proc, void *user_defined_ctx) =
   (int(*)(kv_t *store, const char *key, int klen,
            kv_proc_t kv_proc, void *user_defined_ctx))
   _kv_do;
   return p_kv_do(store, key, klen, kv_proc, user_defined_ctx);
}

extern int gc_init(char *argv[], size_t size)
{
   int(*p_gc_init)(char *argv[], size_t size) =
   (int(*)(char *argv[], size_t size))
   _gc_init;
   p_gc_init(argv, size);
}

extern void *gc_malloc(char *argv[], size_t size)
{
   void*(*p_gc_malloc)(char *argv[], size_t size) =
   (void*(*)(char *argv[], size_t size))
   _gc_malloc;
   p_gc_malloc(argv, size);
}

extern void gc_free(char *argv[], void *ptr)
{
   void(*p_gc_free)(char *argv[], void *ptr) =
   (void(*)(char *argv[], void *ptr))
   _gc_free;
   p_gc_free(argv, ptr);
}

extern void server_report(xbuf_t *reply, int html)
{
   void(*p_server_report)(xbuf_t *reply, int html) =
   (void(*)(xbuf_t *reply, int html))
   _server_report;
   p_server_report(reply, html);
}

extern void mpools_report(xbuf_t *reply)
{
   void(*p_mpools_report)(xbuf_t *reply) =
   (void(*)(xbuf_t *reply))
   _mpools_report;
   p_mpools_report(reply);
}

extern jsn_t *jsn_frtext(char *text, char *name)
{
   jsn_t*(*p_jsn_frtext)(char *text, char *name) =
   (jsn_t*(*)(char *text, char *name))
   _jsn_frtext;
   return p_jsn_frtext(text, name);
}

extern char *jsn_totext(xbuf_t *text, jsn_t *node, int formated)
{
   char*(*p_jsn_totext)(xbuf_t *text, jsn_t *node, int formated) =
   (char*(*)(xbuf_t *text, jsn_t *node, int formated))
   _jsn_totext;
   return p_jsn_totext(text, node, formated);
}

extern jsn_t *jsn_byindex(jsn_t *node, int i)
{
   jsn_t*(*p_jsn_byindex)(jsn_t *node, int i) =
   (jsn_t*(*)(jsn_t *node, int i))
   _jsn_byindex;
   return p_jsn_byindex(node, i);
}

extern jsn_t *jsn_byname(jsn_t *node, char *name, int deep)
{
   jsn_t*(*p_jsn_byname)(jsn_t *node, char *name, int deep) =
   (jsn_t*(*)(jsn_t *node, char *name, int deep))
   _jsn_byname;
   return p_jsn_byname(node, name, deep);
}

extern jsn_t *jsn_byvalue(jsn_t *node, int type, double value, int deep)
{
   jsn_t*(*p_jsn_byvalue)(jsn_t *node, int type, double value, int deep) =
   (jsn_t*(*)(jsn_t *node, int type, double value, int deep))
   _jsn_byvalue;
   return p_jsn_byvalue(node, type, value, deep);
}

extern jsn_t *jsn_add(jsn_t *node, char *name, int type, u64 value)
{
   jsn_t*(*p_jsn_add)(jsn_t *node, char *name, int type, u64 value) =
   (jsn_t*(*)(jsn_t *node, char *name, int type, u64 value))
   _jsn_add;
   return p_jsn_add(node, name, type, value);
}

extern jsn_t *jsn_add_real(jsn_t *node, char *name, double value)
{
   jsn_t*(*p_jsn_add_real)(jsn_t *node, char *name, double value) =
   (jsn_t*(*)(jsn_t *node, char *name, double value))
   _jsn_add_real;
   return p_jsn_add_real(node, name, value);
}

extern jsn_t *jsn_updt(jsn_t *node, double value)
{
   jsn_t*(*p_jsn_updt)(jsn_t *node, double value) =
   (jsn_t*(*)(jsn_t *node, double value))
   _jsn_updt;
   return p_jsn_updt(node, value);
}

extern void jsn_del(jsn_t *node)
{
   void(*p_jsn_del)(jsn_t *node) =
   (void(*)(jsn_t *node))
   _jsn_del;
   p_jsn_del(node);
}

extern void jsn_free(jsn_t *node)
{
   void(*p_jsn_free)(jsn_t *node) =
   (void(*)(jsn_t *node))
   _jsn_free;
   p_jsn_free(node);
}

#endif
// ============================================================================
// End of Source Code
// ============================================================================

