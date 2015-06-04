// ============================================================================
// C servlet header for the G-WAN Web Application Server (http://trustleap.ch/)
// ----------------------------------------------------------------------------
// xbuffer.h: a dynamically growing buffer, with handy functions
// ============================================================================

#ifndef _XBUFFER_H
#define _XBUFFER_H

#ifndef _SHORT_TYPES_H
# include "short_types.h"
#endif 

typedef struct _xbuf_t
{
	char *ptr;       // data buffer
	u32   allocated; // memory allocated
	u32   len;       // memory used
	u32   growby;    // memory allocation increment
} xbuf_t, xbuf_ctx; // xbuf_t is just a shorter alias for xbuf_ctx

// load a file into the buffer
void  xbuf_frfile  (xbuf_t *ctx, char *szFile);

// save the buffer in a file
int  xbuf_tofile  (xbuf_t *ctx, char *szFile);

// allocate more memory to match the requested size
u32   xbuf_growto  (xbuf_t *ctx, u32 len);

// reset the buffer contents and length (but keep memory allocated)
// ctx->len = 0;
// if(ctx->ptr)
//   *ctx->ptr = 0; // in case that's a string, keep it working
void  xbuf_empty   (xbuf_t *ctx);

// return the end of the buffer
// return(ctx->ptr + ctx->len);
char *xbuf_getend  (xbuf_t *ctx);

// attach ctx to the provided buffer
void  xbuf_attach  (xbuf_t *ctx, char *ptr, s32 size, s32 len);

// detach ctx from any previously attached buffer
char *xbuf_detach  (xbuf_t *ctx);

// release the memory allocated for the buffer
void  xbuf_free    (xbuf_t *ctx);

// clear the contents of the buffer
void  xbuf_clear   (xbuf_t *ctx);

// must be called after xbuf_t buf; has been declared to initialize struct:
// ctx->ptr       = NULL;
// ctx->len       = 0;
// ctx->allocated = 0;
// ctx->growby    = PAGE_SIZE;
void  xbuf_init   (xbuf_t *ctx);
void  xbuf_reset  (xbuf_t *ctx); // alias, for compatibility with old code

// copy 'srclen' bytes from 'src' into the buffer
char *xbuf_ncat    (xbuf_t *ctx, char *src, s32 srclen);

// copy the string 'str' into the buffer
char *xbuf_cat     (xbuf_t *ctx, char *str);

// format 'a la sprintf()' into the buffer
char *xbuf_xcat    (xbuf_t *ctx, char *src, ...);

typedef struct
{
   char *ptr;  // data buffer
   u32   len;  // data length
} strtab_t;

// format 'a la writev()' into the buffer
char *xbuf_vcat    (xbuf_t *ctx, const strtab_t *array, int nbr);

// sort text entries separated by 'separator' in the buffer
void  xbuf_sort    (xbuf_t *ctx, char separator, s32 remove_duplicates);

// find the string 'str' in the buffer
char *xbuf_findstr (xbuf_t *ctx, char *str);

// replace all occurences of the 'old' string by the 'new' string in the buffer
char *xbuf_repl    (xbuf_t *ctx, char *old, char *newstr);

// same as above but using a range in the buffer
char *xbuf_replfrto(xbuf_t *ctx, char *beg, char *end, char *old, char *newstr);

// truncate buffer at byte position using pointer 'ptr'
void  xbuf_truncptr(xbuf_t *ctx, char *ptr);

// truncate buffer at byte position using length 'len'
void  xbuf_trunclen(xbuf_t *ctx, s32 len);

// copy up to 'dstlen'-1 bytes of the first available line into 'dst' buffer
// (a line is any number of bytes followed by \n or \r\n)
// return number of bytes read
// return -1 if no complete lines are available
s32   xbuf_getln   (xbuf_t *ctx, char *dst, s32 dstlen);

// move up to 'dstlen' bytes into 'dst' from the top of the buffer
// return number of bytes moved
// return 0 if no data is available
s32   xbuf_pull    (xbuf_t *ctx, char *dst, s32 dstlen);

// move 'len' bytes to 'bytes' from the position 'pos' in the buffer 
void  xbuf_delete  (xbuf_t *ctx, char *pos, s32 len, char *bytes);

// insert 'len' bytes from 'bytes' into the buffer at position 'pos'
s32   xbuf_insert  (xbuf_t *ctx, char *pos, s32 len, char *bytes);

// format a HTTP page from its parts [deprecated]
s32   xbuf_http    (xbuf_t *ctx, s32 code, char *body);

// send an HTTP request to a server, save the reply and return the HTTP status
// ('headers' is adding headers, not overwriting any existing header)
s32   xbuf_frurl   (xbuf_t *ctx, char *host, u32 port, u32 method, char *uri, 
                    u32 mstimeout, char *headers);

#endif // _XBUFFER_H

// ============================================================================
// End of Source Code
// ============================================================================
